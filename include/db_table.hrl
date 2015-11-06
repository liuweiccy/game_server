-ifndef(DB_TABLE_HRL).
-define(DB_TABLE_HRL, db_table_hrl).

%% @doc 数据库建表信息
-record(table_spec, {name, type, keypos, file}).

%% @doc 玩家表
-define(DB_PLAYER, db_player).
-record(db_player, {
    id = 0,
    name = "",
    version = 0,
    sex = 0,
    career = 0,
    level = 1,
    exp = 0,
    coin = 0,
    rmb = 0,
    create_time = 0,
    last_logout_time = 0,
    map_id = 0,
    pos_x = 0,
    pos_y = 0
}).

%% @doc 账号表
-define(DB_ACCOUNT, db_account).
-record(db_account, {
    account,
    account_type = 0,
    create_time = 0,
    player_list = []
}).

-endif.

