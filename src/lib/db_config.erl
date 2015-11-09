%%%-------------------------------------------------------------------
%%% @author 503407245@qq.com
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% 数据库配置，存储在本地磁盘
%%% @end
%%% Created : 04. 十一月 2015 11:03
%%%-------------------------------------------------------------------
-module(db_config).

-include("db_table.hrl").

%% API
-export([tables/0]).

tables() -> [
    #table_spec{
        name = ?DB_ACCOUNT,
        type = set,
        file = "account.DCD",
        keypos = #db_account.account},

    #table_spec{
        name = ?DB_PLAYER,
        type = set,
        file = "player.DCD",
        keypos = #db_player.id}
    ].
