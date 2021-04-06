-ifndef(GAME_RANK_H).
-define(GAME_RANK_H, true).

-define(GPROC_RANK_NAME(RankName), {rank, RankName}).
-record(rank_map, {
    key :: term(),          % {RankName, Rank :: game_rank:rank()}
    record_id :: term(),    % 排行主键id
    score :: term()         % 排行值
}).

%% 存储 {RankName, RankList}, 获取排名列表
%% 存储 {{min_score, RankName}, MinScore}, 获取最小分数
%% 存储 {{options, RankName}, Options}, 获取排行配置
-define(ETS_RANK_LIST, ets_rank_list).

%% 存储 #rank_map{}, 方便找到key对应的排名
-define(ETS_RANK_MAP, ets_rank_map).

-endif.
