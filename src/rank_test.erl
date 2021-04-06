-module(rank_test).

-record(user, {
    user_id = 0,
    user_name = <<"">>,
    sex = 1,
    scores = 0
}).

-export([
    recordset_identity/2,
    recordset_sort/2,
    recordset_options/1,
    record_id/1,
    record_score/1,
    record_id_and_score/1,

    start/0,
    add_data/0,
    add_data/1,
    add_data/2
]).

recordset_identity(User1, User2) ->
    User1#user.user_id == User2#user.user_id.

recordset_sort(User1, User2) ->
    User1#user.scores =< User2#user.scores.

recordset_options(1) ->
    [{save_interval, 60 * 5 * 1000}, {max_size, 200}].

record_id(User) -> User#user.user_id.

record_score(User) -> User#user.scores.

record_id_and_score(User) -> {User#user.user_id, User#user.scores}.


start() ->
    game_rank:start(1, ?MODULE).

add_data(UserId, Scores) ->
    User = #user{user_id = UserId, user_name = game_util:to_binary(UserId), scores = Scores},
    game_rank:add_rank(1, ?MODULE, User).

add_data() ->
    UserId = game_util:random(1, 1000),
    User = #user{user_id = UserId, user_name = game_util:to_binary(UserId), scores = game_util:random(1, 1000)},
    game_rank:add_rank(1, ?MODULE, User).

add_data(0) -> ok;
add_data(Num) ->
    add_data(),
    add_data(Num - 1).
