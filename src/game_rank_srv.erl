%%%-------------------------------------------------------------------
%%% @doc
%%% 排行榜进程模块
%%% @end
%%%-------------------------------------------------------------------
-module(game_rank_srv).

-behaviour(gen_server).

-include("game_rank.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/1,
    add_rank/2,
    cover_recordset/2,
    sync_rank/1
]).

%% options:
%%      [{
%%          save_interval, Interval :: integer() % ms, zero means save now when data change
%%      }]
-record(state, {
    rank_name :: game_rank:rank_name(),
    module :: module(),
    recordset :: recordset:recordset(),
    is_change = false :: boolean(),
    options = [] :: list(),
    save_interval_ref = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc 增加
-spec add_rank(game_rank:rank_name(), term()) -> ok.
add_rank(RankName, Record) ->
    Pid = gproc:lookup_local_name(?GPROC_RANK_NAME(RankName)),
    gen_server:cast(Pid, {add_rank, Record}).

%% @doc 覆盖排行榜数据
-spec cover_recordset(game_rank:rank_name(), recordset:recordset()) -> ok.
cover_recordset(RankName, RecordSet) ->
    Pid = gproc:lookup_local_name(?GPROC_RANK_NAME(RankName)),
    gen_server:cast(Pid, {cover, RecordSet}).

%% @doc 同步排行榜数据库
-spec sync_rank(game_rank:rank_name()) -> ok.
sync_rank(RankName) ->
    Pid = gproc:lookup_local_name(?GPROC_RANK_NAME(RankName)),
    gen_server:cast(Pid, sync_rank).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([RankName, Module, RecordSet0, Options]) ->
    process_flag(trap_exit, true),
    gproc:add_local_name(?GPROC_RANK_NAME(RankName)),
    % RecordSet = game_rank_mnesia:load_recordset(RankName, RecordSet0),
    RecordSet = RecordSet0,
    insert_rank_list(RankName, RecordSet),
    insert_rank_maps(RankName, Module, RecordSet),
    init_min_score(RankName, Module, RecordSet),
    {ok, #state{
        rank_name = RankName,
        module = Module,
        recordset = RecordSet,
        options = Options
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% 排行
handle_cast({add_rank, Rank}, #state{
        rank_name = RankName,
        module = Module,
        options = Options,
        save_interval_ref = Ref
    } = State) ->
    RecordSet = recordset:add(Rank, State#state.recordset),
    insert_rank_list(RankName, RecordSet),
    insert_rank_maps(RankName, Module, RecordSet),
    insert_min_score(RankName, Module, RecordSet),
    NewState = case proplists:get_value(save_interval, Options, 0) of
        0 ->
            % 即时保存
            % game_rank_mnesia:save_rank_list(RankName, RecordSet),
            State;
        SaveInterval ->
            % 定时保存
            NewRef = case Ref of
                undefined ->
                    erlang:start_timer(SaveInterval, self(), save_rank);
                _ ->
                    Ref
            end,
            State#state{save_interval_ref = NewRef, is_change = true}
    end,
    {noreply, NewState#state{recordset = RecordSet}};
%% 覆盖排行表
handle_cast({cover, RecordSet}, #state{rank_name = RankName, module = Module} = State) ->
    insert_rank_list(RankName, RecordSet),
    clear_rank_map(RankName),
    insert_rank_maps(RankName, Module, RecordSet),
    init_min_score(RankName, Module, RecordSet),
    % game_rank_mnesia:save_rank_list(RankName, RecordSet),
    {noreply, State#state{recordset = RecordSet}};
%% 手动保存榜单
handle_cast(sync_rank, #state{rank_name = _RankName, recordset = _RecordSet} = State) ->
    % game_rank_mnesia:save_rank_list(RankName, RecordSet),
    {noreply, State#state{is_change = false}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% 定时保存榜单
handle_info({timeout, Ref, save_rank}, #state{
        save_interval_ref = Ref,
        is_change = true
    } = State) ->
    % game_rank_mnesia:save_rank_list(State#state.rank_name, State#state.recordset),
    {noreply, State#state{is_change = false, save_interval_ref = undefined}};
handle_info({timeout, _Ref, save_rank}, #state{} = State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{rank_name = RankName, recordset = _RecordSet}) ->
    % game_rank_mnesia:save_rank_list(RankName, RecordSet),
    clear_rank_list(RankName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_rank_list(RankName, RecordSet) ->
    RankList = recordset:to_list(RecordSet),
    ets:insert(?ETS_RANK_LIST, {RankName, RankList}).

clear_rank_list(RankName) ->
    ets:delete(?ETS_RANK_LIST, RankName),
    ets:delete(?ETS_RANK_LIST, {min_score, RankName}),
    ets:delete(?ETS_RANK_LIST, {options, RankName}),
    clear_rank_map(RankName),
    ok.

%% @doc 初始化最后一名的分数
init_min_score(RankName, Module, RecordSet) ->
    case recordset:size(RecordSet) == recordset:max_size(RecordSet) of
        true ->
            case recordset:to_list(RecordSet) of
                [] ->
                    insert_min_score_do(RankName, 0);
                [Record | _] ->
                    Score = Module:record_score(Record),
                    insert_min_score_do(RankName, Score)
            end;
        false -> % 还没有达到max,并不需要设置最小值
            insert_min_score_do(RankName, 0)
    end.

%% @doc 保存最后一名的分数
insert_min_score(RankName, Module, RecordSet) ->
    case recordset:size(RecordSet) == recordset:max_size(RecordSet) of
        true ->
            case recordset:to_list(RecordSet) of
                [] ->
                    ok;
                [Record | _] ->
                    Score = Module:record_score(Record),
                    insert_min_score_do(RankName, Score)
            end;
        false ->
            ok
    end.

insert_min_score_do(RankName, Score) ->
    ets:insert(?ETS_RANK_LIST, {{min_score, RankName}, Score}).

insert_rank_map(Objects) ->
    ets:insert(?ETS_RANK_MAP, Objects).

insert_rank_maps(RankName, Module, RecordSet) ->
    List = recordset:to_list(RecordSet),
    {Objects, _} = lists:mapfoldr(fun(RankRecord, Rank) ->
        {RecordId, RecordScore} = Module:record_id_and_score(RankRecord),
        {#rank_map{key = {RankName, Rank}, record_id = RecordId, score = RecordScore}, Rank + 1}
    end, 1, List),
    insert_rank_map(Objects).

clear_rank_map(RankName) ->
    ets:match_delete(?ETS_RANK_MAP, #rank_map{key = {RankName, '_'}, record_id = '_', score = '_'}).
