%%%-------------------------------------------------------------------
%%% @author 503407245@qq.com
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% 数据库接口
%%% @end
%%% Created : 09. 十一月 2015 11:06
%%%-------------------------------------------------------------------
-module(db_interface).

-include("db_table.hrl").

%% API
-export([init/0, close/0]).


%% 初始化数据库表
init() ->
    {ok, DbPath} = application:get_env(db_path),
    lists:foreach(
        fun(#table_spec{name = TableName, keypos = KeyPos, type = Type, file = FileName}) ->
            {ok, _} = dets:open_file(TableName, [{type, Type}, {keypos, KeyPos}, {file, filename:absname_join(DbPath, FileName)}])
        end, db_config:tables()), ok.

%% 关闭数据表
close() ->
    lists:foreach(
        fun(#table_spec{name = TableName}) ->
            dets:close(TableName)
        end), ok.