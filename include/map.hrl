%%
%% 地图定义
%%

-define(MAP_TYPE_FIXED, 1). 									% 固定地图
-define(MAP_TYPE_ALLOCATE, 2).									% 分线地图
-define(MAP_TYPE_DUNGEON, 3).									% 副本地图

-define(MAP_FRAME_RATE, 200).									% 地图帧频


-record(map_index, {
				    main_map_id = 0,   										% 分线地图 主地图ID
				    map_indexs = []											% 包括主地图ID 在内的 所有分线地图ID
				    }).

-record(map_grid, {
				    player_list = [],											% 玩家列表
				    mon_list = []												% 怪物列表
				    }).

-define(OBJECT_TYPE_PLAYER, 1).

%% 对象信息
-record(object_info, {
					 object_id = 0,								% 对象ID
					 object_type = 0,							% 对象类型(1 玩家 2 怪物 3 宠物)
					 own_id = 0,								% 拥有者ID
					 own_type = 0								% 拥有者类型
					 }).

%% 地图转场事件类型(主要在离场传入此标志)
-define(BEHAVIOR_LOGOUT, 0).										% 下线
-define(BEHAVIOR_CHANGE_MAP, 1).							        % 转场

%% 地图模版的各类触发性事件
-define(MAP_EVENT_PLAYER_ENTER(PlayerStatus, Behaviour), {player_enter, PlayerStatus, Behaviour}).
-define(MAP_EVENT_PLAYER_LEAVE(PlayerID, Behaviour), {player_leave, PlayerID, Behaviour}).