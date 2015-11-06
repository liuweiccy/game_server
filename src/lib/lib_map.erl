%% @author ASUS
%% @doc @todo Add description to lib_map.


-module(lib_map).

-include("common.hrl").
-include("map.hrl").
-include("data_record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 init/0,
		 get_map_id/0, 
		 set_map_id/1]).

-export([
		 defaule_map_pos/0,
		 get_map_increase_id/1, 
		 get_map_doc_id/1,
		 resolve_map_id/1,
		 resolve_map_doc_id/1,
		 get_map_enter_pos/1]).

-export([
		 add_player/1, 
		 get_map_player_ids/0, 
		 del_player/1]).

defaule_map_pos() ->
	{11, 12, 22}.

init() ->
	lists:foreach(
	  fun(MapDocID) ->
			  case data_map:get(MapDocID) of 
				  #map_template{map_type = ?MAP_TYPE_FIXED} -> %% 开服启动固定地图
					  {MapModule, _MapType} = resolve_map_doc_id(MapDocID),
					  NewMapID = MapDocID * ?TEN_MILLION,
					  {ok, _Pid} = sup_map:start_child([NewMapID, MapDocID, MapModule]);
				  _ ->
					  skip
			  end
	  end, data_map:get_ids()).

-define(MAP_ID, {?MODULE, map_id}).
get_map_id() ->
	util:get_dict(?MAP_ID, 0).

set_map_id(MapID) ->
	util:set_dict(?MAP_ID, MapID).

%% 获取模版ID
get_map_doc_id(MapID) ->
	MapID div ?TEN_MILLION.

%% 获取地图自增ID
get_map_increase_id(MapDocID) ->
	IncreaseID = lib_increase:map_id(MapDocID),
	MapDocID * ?TEN_MILLION + IncreaseID.

%% 根据MapId 获取详细信息
resolve_map_id(MapID) ->
	MapDocID = get_map_doc_id(MapID),
	#map_template{map_type = MapType} = data_map:get(MapDocID),
	MapModule = map_template:get_module(MapType),
	{MapDocID, MapModule, MapType}.

resolve_map_doc_id(MapDocID) ->
	#map_template{map_type = MapType} = data_map:get(MapDocID),
	MapModule = map_template:get_module(MapType),
	{MapModule, MapType}.

%% 地图添加/删除玩家
add_player(PlayerID) ->
	NewPlayerIDS = util:add_set(PlayerID, get_map_player_ids()),
	set_map_player_ids(NewPlayerIDS).

del_player(PlayerID) ->
	NewPlayerIDS = lists:delete(PlayerID, get_map_player_ids()),
	set_map_player_ids(NewPlayerIDS).

%% 获取地图玩家列表
-define(MAP_PLAYER_IDS, {?MODULE, map_player_ids}).
get_map_player_ids() ->
	util:get_dict(?MAP_PLAYER_IDS, []).
set_map_player_ids(List) when is_list(List) ->
	util:set_dict(?MAP_PLAYER_IDS, List).

%% 获取地图进入点
get_map_enter_pos(MapDocID) ->
	#map_template{enter_pos = EnterPos} = data_map:get(MapDocID),
	EnterPos.

%% ====================================================================
%% Internal functions
%% ====================================================================


