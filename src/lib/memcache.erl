%% @author jiangxiaowei
%% @doc 游戏数据和缓存表
-module(memcache).

-include("player.hrl").
-include("common.hrl").
-include("map.hrl").
-include("ets.hrl").
-include("db_table.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([create_ets/0, create_dets/0, close_dets/0]).

%% 内存逻辑表
create_ets() ->
	ets:new(?ETS_PLAYER_ONLINE, [set, public, named_table, {keypos, #player_online.player_id}, {write_concurrency, true}, {read_concurrency, true}]),
	ok.

%% 磁盘表(数据库)
create_dets() ->
    {ok, DbPath} = application:get_env(db_path),
    lists:foreach(
    fun(#table_spec{name = TableName, keypos = KeyPos, type = Type, file = FileName}) ->
        {ok, _} = dets:open_file(TableName, [{type, Type}, {keypos, KeyPos}, {file, filename:absname_join(DbPath, FileName)}])
    end, db_config:tables()), ok.

close_dets() ->
    lists:foreach(
    fun(#table_spec{name = TableName}) ->
        dets:close(TableName)
    end), ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


			
