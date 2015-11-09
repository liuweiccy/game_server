%% @author 503407245@qq.com
%% @doc 数据缓存表
-module(memcache).

-include("player.hrl").
-include("common.hrl").
-include("map.hrl").
-include("ets.hrl").
-include("db_table.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([create_ets/0]).

%% 内存逻辑表
create_ets() ->
	ets:new(?ETS_PLAYER_ONLINE, [set, public, named_table, {keypos, #player_online.player_id}, {write_concurrency, true}, {read_concurrency, true}]),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


			
