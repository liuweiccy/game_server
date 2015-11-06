%% @author ASUS
%% @doc @todo Add description to map_config.


-module(map_template).

-include("map.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_module/1]).

get_module(?MAP_TYPE_FIXED) -> %% 固定地图
	fixed_map;
%% get_module(?MAP_TYPE_ALLOCATE) -> %% 分线地图
%% 	allocate_map;
%% get_module(?MAP_TYPE_DUNGEON) -> %% 副本地图
%% 	dungeon_map;
get_module(_MapType) ->
	throw(nomatch_map_module).

%% ====================================================================
%% Internal functions
%% ====================================================================


