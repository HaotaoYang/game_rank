%%%-------------------------------------------------------------------
%%% @doc
%%% 排行榜模块
%%% @end
%%%-------------------------------------------------------------------
-module(game_rank).

-include("game_rank.hrl").

%% API
-export([
    start/2,
    stop/0, stop/1,
    add_rank/3,
    get_rank_by_key/2,
    get_rank_list/3,
    get_rank_list/1,
    reset_rank_list/2,
    new_recordset/2
]).

-export_type([rank/0, rank_name/0]).
-type rank() :: pos_integer().
-type rank_name() :: pos_integer() | tuple().

-callback recordset_identity(term(), term()) -> boolean().
-callback recordset_sort(term(), term()) -> boolean().
-callback recordset_options(rank_name()) -> proplists:proplist().
-callback record_id(RankRecord :: term()) -> term().
-callback record_score(RankRecord :: term()) -> integer().
-callback record_id_and_score(RankRecord :: term()) -> {term(), integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 开启排行榜
-spec start(rank_name(), module()) -> supervisor:startchild_ret().
start(RankName, Module) ->
    RecordSet = new_recordset(Module, RankName),
    Options = get_options(RankName),
    game_rank_sup:start_child([RankName, Module, RecordSet, Options]).

%% @doc 关闭排行榜
-spec stop() -> ok.
stop() ->
    game_rank_sup:stop_all().
-spec stop(rank_name()) -> ok.
stop(RankName) ->
    ChildPid = gproc:lookup_local_name(?GPROC_RANK_NAME(RankName)),
    game_rank_sup:stop_child(ChildPid).

%% @doc 插入排行榜数据
-spec add_rank(rank_name(), module(), term()) -> ok.
add_rank(RankName, Module, Record) ->
    case Module:record_score(Record) of
        0 -> ok;
        Score ->
            MinScore = get_min_score(RankName),
            case Score > MinScore of
                true ->
                    game_rank_srv:add_rank(RankName, Record);
                _ -> ok
            end
    end.

%% @doc 根据key获得对应排名
-spec get_rank_by_key(rank_name(), RankId :: term()) -> rank().
get_rank_by_key(RankName, RankId) ->
    % Key = {RankName, Rank}
    Pattern = #rank_map{key = {RankName, '$1'}, record_id = RankId, score = '_'},
    case ets:match(?ETS_RANK_MAP, Pattern) of
        [[Rank]] -> Rank;
        _ -> 0
    end.

%% @doc 获取排行列表(分页)
-spec get_rank_list(rank_name(), Page :: pos_integer(), PageLen :: pos_integer()) ->
    {Total :: pos_integer(), Pages :: pos_integer(), OnPage :: list()}.
get_rank_list(RankName, Page, PageLen) ->
    List = get_rank_list(RankName),
    game_util:paging(lists:reverse(List), Page, PageLen).

%% @doc 获取排行列表
-spec get_rank_list(rank_name()) -> list().
get_rank_list(RankName) ->
    case ets:lookup(?ETS_RANK_LIST, RankName) of
        [{_, RankList}] -> RankList;
        _ -> []
    end.

%% @doc 清空排行榜
-spec reset_rank_list(rank_name(), module()) -> ok.
reset_rank_list(RankName, Module) ->
    RecordSet = new_recordset(Module, RankName),
    game_rank_srv:cover_recordset(RankName, RecordSet).

%% @doc 构建recordset结构
-spec new_recordset(module(), rank_name()) -> recordset:recordset().
new_recordset(Module, RankName) ->
    IdentityFun = fun Module:recordset_identity/2,
    SortFun = fun Module:recordset_sort/2,
    Options = Module:recordset_options(RankName),
    % 排行配置
    ProcOptions = normalize(Options, []),
    set_options(RankName, ProcOptions),
    recordset:new(IdentityFun, SortFun, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_min_score(rank_name()) -> pos_integer().
get_min_score(RankName) ->
    case ets:lookup(?ETS_RANK_LIST, {min_score, RankName}) of
        [{_, Score}] -> Score;
        _ -> 0
    end.

%% 查询榜单参数
get_options(RankName) ->
    case ets:lookup(?ETS_RANK_LIST, {options, RankName}) of
        [{_, Options}] -> Options;
        _ -> []
    end.

%% 设置榜单参数
set_options(RankName, Options) ->
    ets:insert(?ETS_RANK_LIST, {{options, RankName}, Options}).

%% 榜单参数
normalize([{save_interval, Value}|T], Res) when is_integer(Value) ->
    normalize(T, [{save_interval, Value}|Res]);
normalize([_|T], Res) ->
    normalize(T, Res);
normalize([], Res) ->
    Res.
