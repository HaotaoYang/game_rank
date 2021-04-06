%%%-------------------------------------------------------------------
%%% @doc
%%% 排行榜supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(game_rank_sup).

-behaviour(supervisor).

-include("game_rank.hrl").

%% API
-export([
    start_link/0,
    start_child/1,
    stop_child/1,
    stop_all/0
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(Args :: list()) -> supervisor:startchild_ret().
start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

-spec stop_child(ChildPid :: pid()) -> ok.
stop_child(ChildPid) ->
    supervisor:terminate_child(?MODULE, ChildPid).

-spec stop_all() -> ok.
stop_all() ->
    [stop_child(Child) || {_Id, Child, _Type, _Modules} <- supervisor:which_children(?MODULE)],
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    % 排行榜列表
    ets:new(?ETS_RANK_LIST, [named_table, public, set, {read_concurrency, true}]),
    % key对应排行榜的排行
    ets:new(?ETS_RANK_MAP, [named_table, public, set, {read_concurrency, true}, {keypos, #rank_map.key}]),

    SupFlags = #{strategy => simple_one_for_one, intensity => 1000, period => 3600},
    Children = [
        #{id => game_rank_srv, start => {game_rank_srv, start_link, []}, modules => [game_rank_srv],
            restart => permanent, shutdown => 2000, type => worker}
    ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
