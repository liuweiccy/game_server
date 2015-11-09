%% @author 503407245@qq.com
%% @doc lib_player.


-module(lib_player).

-include("player.hrl").
-include("common.hrl").
-include("db_table.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_register_name/1, get_id/0, set_id/1, get_id/1, get_socket/0, set_socket/1]).

-export([on_enter/1, on_save/1]).

-export([get_base_info/0, set_base_info/1,
         get_map_info/0, set_map_info/1,
         get_currency_info/0, set_currency_info/1,
         get_level_info/0, set_level_info/1]).

-export([go_new_map/5,go_new_map/3]).

-export([leave_map/5, leave_map/2, enter_map/1]).

get_id() ->
	util:get_dict(player_id, 0).
set_id(PlayerID) ->
	util:set_dict(player_id, PlayerID).

get_id(#player_state{player_id = PlayerID}) ->
	PlayerID.

get_register_name(PlayerID) ->
    util:register_name(player, PlayerID).

get_socket() ->
    util:get_dict(player_socket, undefined).
set_socket(Socket) ->
    util:set_dict(player_socket, Socket).

get_base_info() ->
	util:get_dict(?PLAYER_BASE_INFO, none).
set_base_info(BaseInfo) when is_record(BaseInfo, player_base_info)->
	util:set_dict(?PLAYER_BASE_INFO, BaseInfo).

get_map_info() ->
    util:get_dict(?PLAYER_MAP_INFO, none).
set_map_info(MapInfo) when is_record(MapInfo, player_map_info)->
    util:set_dict(?PLAYER_MAP_INFO, MapInfo).

get_currency_info() ->
    util:get_dict(?PLAYER_CURRENCY_INFO, none).
set_currency_info(CurrencyInfo) when is_record(CurrencyInfo, player_currency_info)->
    util:set_dict(?PLAYER_CURRENCY_INFO, CurrencyInfo).

get_level_info() ->
    util:get_dict(?PLAYER_LEVEL_INFO, none).
set_level_info(LevelInfo) when is_record(LevelInfo, player_level_info)->
    util:set_dict(?PLAYER_LEVEL_INFO, LevelInfo).

%% 转场接口
go_new_map(PlayerId, MapDocID, Behaviour) ->
	{PosX, PosY} = lib_map:get_map_enter_pos(MapDocID),
	go_new_map(PlayerId, MapDocID, PosX, PosY, Behaviour).

go_new_map(PlayerId, MapDocID, PosX, PosY, Behaviour) ->
	gen_server_boot:invoke_async(lib_player:get_register_name(PlayerId),
						 fun(PlayerStatus) ->
								 NewPlayerStatus = leave_map(MapDocID, PosX, PosY, Behaviour, PlayerStatus),
								 {ok, NewPlayerStatus}
						 end).

%% 离场
leave_map(#player_state{player_id = PlayerID}, Behaviour) ->
    #player_map_info{map_id = MapID} = get_map_info(),
	proc_map:leave(MapID, PlayerID, Behaviour).
leave_map(EnterMapDocID, PosX, PosY, Behaviour, #player_state{player_id = PlayerID, is_change_map = false} = PlayerStatus) ->
    #player_map_info{map_id = MapID} = get_map_info(),
    case catch proc_map:leave(MapID, PlayerID, Behaviour) of
		ok ->
			{MapModule, _MapType} = lib_map:resolve_map_doc_id(EnterMapDocID),
			%% 分配要进入的场景信息
			%% 此处决定最终要去的场景ID 和 坐标
			%% 返回的NewMapID的地图进程必须是存在的
			case catch MapModule:allocate_map(PlayerStatus, EnterMapDocID, PosX, PosY) of 
				{NewMapID, NewPosX, NewPosY} -> 
					{_NewMapDocID, _NewMapModule, _NewMapType} = lib_map:resolve_map_id(NewMapID),
                    set_map_info(#player_map_info{map_id = NewMapID, pos_x = NewPosX, pos_y = NewPosY}),
					%% 发送协议通知客户端切换场景
					FinalPlayerStatus = PlayerStatus#player_state{is_change_map = true},
					FinalPlayerStatus;
				_Other->
					?ERROR("地图模块[~w] 分配场景地图失败, 地图模版ID[~w], 描述:~w",[MapModule, EnterMapDocID, _Other]),
					PlayerStatus
			end;
		_Other ->
			?ERROR("玩家[~w] 离开地图[~w] 失败, 描述:~w",[PlayerID, MapID, _Other]),
			PlayerStatus
	end.

%% 入场
enter_map(#player_state{player_id = PlayerID, is_change_map = true} = PlayerStatus) ->
    #player_map_info{map_id = MapID, pos_x = PosX, pos_y = PosY} = get_map_info(),
    case catch proc_map:enter(MapID, PosX, PosY, PlayerStatus) of
		ok ->
			FinalPlayerStatus = PlayerStatus#player_state{is_change_map = false},
			%% ETS_PLAYER_ONLINE 中的玩家数据只有在进场的时候会重构一次．(insert or update)
			%% ETS表中查询不到的玩家都可以认为是不在线状态
            %% is_change_map = true 的玩家表示在线, 但是转场状态, 场景相关的协议可以不发送
			FinalPlayerStatus;
		_Other ->
			?ERROR("玩家[~w] 进入场景[~w] 失败, 描述:~w",[PlayerID, MapID, _Other]),
			PlayerStatus
	end.

%% --------------------------------------------------------------------------------------
%% 玩家上线
%% --------------------------------------------------------------------------------------
on_enter(PlayerID) ->
	[DbUser] =  dets:lookup(?DB_PLAYER, PlayerID),
	PlayerStatus = init_data(DbUser),
	NewPlayerStatus =  check_reset(PlayerStatus),
	?DEBUG("玩家[~w] 进程数据初始化完成",[PlayerID]),
	erlang:send_after(?PLAYER_LOOP_TIME, self(), player_loop),
    lib_online:add_player(NewPlayerStatus),
    {ok, NewPlayerStatus}.

init_data(#db_player{
					id = PlayerID,
					version = Version,
					name = Name,
					sex = Sex,
					level = Level,
                    exp = Exp,
                    rmb = Rmb,
                    coin = Coin,
                    career = Career,
					create_time = CreateTime,
					map_id = MapId,
					pos_x = PosX,
					pos_y = PosY,
                    last_logout_time = LastLogoutTime}) ->
	%% PID
	true = erlang:register(util:register_name(player, PlayerID), self()),
	
	%% ID
	util:set_dict(player_id, PlayerID),
	
	%% BaseInfo
	BaseInfo = #player_base_info{name = Name, sex = Sex, career = Career, create_time = CreateTime},
	set_base_info(BaseInfo),

    %% CurrencyInfo
    CurrencyInfo = #player_currency_info{rmb = Rmb, coin = Coin},
    set_currency_info(CurrencyInfo),

    %% LevelInfo
	LevelInfo = #player_level_info{level = Level, exp = Exp},
    set_level_info(LevelInfo),

	%% 地图重定向, 决定玩家上线最终所在地图
	{_MapDocID, MapModule, _MapType} = lib_map:resolve_map_id(MapId),
    MapInfo = #player_map_info{map_id = MapId, pos_x = PosX, pos_y = PosY},
    NewMapInfo = MapModule:init_redirect(PlayerID, MapInfo),
    set_map_info(NewMapInfo),

    #player_state{
        player_id = PlayerID,
        version = Version,
        login_step = ?P_LOGIN_STATUS_GAME,
        last_logout_time = LastLogoutTime}.

check_reset(#player_state{last_logout_time = LastlogoutTime} = PlayerStatus) ->
	case util:is_same_day(LastlogoutTime) of
		true ->
			PlayerStatus;
		false ->
			on_reset(PlayerStatus)
	end.

%% --------------------------------------------------------------------------------------
%% 日常数据重置
%% --------------------------------------------------------------------------------------
on_reset(PlayerStatus) ->
	%% 重置数据代码请在此之前
	PlayerStatus.

%% --------------------------------------------------------------------------------------
%% 玩家下线 ,  数据保存相关都在此处
%% --------------------------------------------------------------------------------------
on_save(PlayerStatus) ->
	DbUser = convert_save_data(PlayerStatus),
	SaveDbUser = DbUser#db_player{last_logout_time = util:unixtime()},
	dets:insert(?DB_PLAYER, SaveDbUser),
	ok.
	
convert_save_data(#player_state{
						player_id = PlayerID,
						version = Version}) ->
	#player_base_info{name = Name, sex = Sex, career = Career, create_time = CreateTime} = get_base_info(),
    #player_map_info{map_id = MapId, pos_x = PosX, pos_y = PosY} = get_map_info(),
    #player_level_info{level = Level, exp = Exp} = get_level_info(),
    #player_currency_info{rmb = Rmb, coin = Coin} = get_currency_info(),
	#db_player{
			id = PlayerID,
			version = Version,
			name = Name,
			sex = Sex,
            career = Career,
			level = Level,
            exp = Exp,
			create_time = CreateTime,
			map_id = MapId,
			pos_x = PosX,
			pos_y = PosY,
            rmb = Rmb,
            coin = Coin}.

%% ====================================================================
%% Internal functions
%% ====================================================================


