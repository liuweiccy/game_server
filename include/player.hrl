-ifndef(PLAYER_HRL).
-define(PLAYER_HRL, player_hrl).

%% 进程状态
-define(P_LOGIN_STATUS_NORMAL, 0).
-define(P_LOGIN_STATUS_LOGIN, 1).
-define(P_LOGIN_STATUS_CREATE, 2).
-define(P_LOGIN_STATUS_GAME, 3).

%% 进程参数
-define(PLAYER_LOOP_TIME, 1000).

%% 玩家进程state
-record(player_state, {
					player_id = 0,
					version = 0,
					socket = none,
		            login_step  = 0,
                    is_change_map = true,
                    last_logout_time = 0
					}).

%% %% 在线玩家信息
-record(player_online, {
					player_id = 0,
					name,
					career = 0,
					sex = 0,
					level = 1,
					map_id = 0
					}).

%% ==========================================================================================
%% 玩家进程字典
%% ==========================================================================================

%% 玩家账号信息
-define(PLAYER_ACCOUNT_INFO, player_account_info).
-record(player_account_info, {account, account_type}).

%% 玩家基础信息
-define(PLAYER_BASE_INFO, player_base_info).
-record(player_base_info, {
					name,
					career = 0,
                    sex = 0,
					create_time = 0
					}).

%% 玩家位置信息
-define(PLAYER_MAP_INFO, player_map_info).
-record(player_map_info, {
                    map_id = 0,
                    pos_x = 0,
                    pos_y = 0
                    }).

%% 玩家货币信息
-define(PLAYER_CURRENCY_INFO, player_currency_info).
-record(player_currency_info, {
                    coin = 0,
                    rmb = 0
                    }).

%% 玩家等级信息
-define(PLAYER_LEVEL_INFO, player_level_info).
-record(player_level_info, {
                    level = 0,
                    exp = 0
                    }).


-endif.