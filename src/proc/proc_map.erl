%% @author 503407245@qq.com
%% @doc 地图进程
%% 地图底层代码。主要实现所有地图所拥有的公共方法，作为所有地图类型的父类
%% 所有具体地图通过 实现map_template的地图回调 实现具体复杂逻辑
%% 地图类型对应的处理模块在map_template.erl 添加
%% 地图只处理ai，对象管理，格子管理， 战斗由玩家进程本身处理，负载均衡

-module(proc_map).
-behaviour(gen_server).

-include("common.hrl").
-include("map.hrl").
-include("player.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3, stop/1]).

-export([enter/5, leave/3, trigger/2]).

start_link(MapID, MapDocID, MapModule) ->
    gen_server_boot:start_link(?MODULE, [MapID,MapDocID, MapModule], []).

stop(MapID) when erlang:is_integer(MapID) ->
	stop(lib_map:get_register_name(MapID));
stop(RegName) when erlang:is_atom(RegName) ->
	case is_alive(RegName) of
		true ->
			gen_server:cast(RegName, stop);
		false ->
			skip
	end.

is_alive(RegName) when erlang:is_atom(RegName) ->
	erlang:whereis(RegName) =/= undefined;
is_alive(MapID) when erlang:is_integer(MapID)->
	erlang:whereis(lib_map:get_register_name(MapID)) =/= undefined.

enter(MapID, PosX, PosY, Behaviour, PlayerStatus) ->
	gen_server:call(lib_map:get_register_name(MapID), {enter, PosX, PosY, Behaviour, PlayerStatus}).

leave(MapID, PlayerID, Behaviour) ->
	gen_server:call(lib_map:get_register_name(MapID), {leave, PlayerID, Behaviour}).

trigger(MapID, Msg) ->
	gen_server:cast(lib_map:get_register_name(MapID), {trigger, Msg}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(map_state, {map_id = 0,  map_doc_id = 0, module = undefined, module_state = undefined}).
-define(MAP_LOOP_TIME, 200).

init([MapID, MapDocID, MapModule]) ->
	case catch MapModule:on_init(MapID, MapDocID) of
        {ok, NewState} ->
			%% 注册地图进程名,   map ++ MapID 
			true = erlang:register(lib_map:get_register_name(MapID), self()),
			%% 启动地图循环
			erlang:send_after(?MAP_LOOP_TIME, self(),  map_loop),
			?DEBUG("地图ID[~w] 地图模块[~w]  启动成功",[MapID, MapModule]),
			{ok, #map_state{map_id = MapID, map_doc_id = MapDocID, module = MapModule, module_state = NewState}};
		_Other ->
			?ERROR("地图ID[~w] 地图模块[~w] 进程初始化失败, 描述:~w",[MapID, MapModule,_Other]),
			{stop, normal}
	end.
    
%% ==========  handle_call/3 =====================
handle_call({enter, _PosX, _PosY, Behaviour, PlayerStatus = #player_state{player_id = PlayerID}}, _From,
			MapState = #map_state{map_id = MapID,  map_doc_id = MapDocID, module = MapModule, module_state = ModuleState}) ->
	%% 添加玩家到地图字典
	lib_map:add_player(PlayerID),
	%% 通知周围玩家 - 玩家进入
	%% 地图模版MAP_EVENT_PLAYER_ENTER 事件
	NewModuleState = 
		case catch MapModule:on_trigger(?MAP_EVENT_PLAYER_ENTER(PlayerStatus, Behaviour), MapID, MapDocID, ModuleState) of
            {ok, NewState} ->
				NewState;
			_ ->
				ModuleState
		end,
	{reply, ok, MapState#map_state{module_state = NewModuleState}};
handle_call({leave, PlayerID, Behaviour}, _From, MapState = #map_state{map_id = MapID, map_doc_id = MapDocID, module = MapModule, module_state = ModuleState}) ->
	%% 移除玩家在地图信息
	lib_map:del_player(PlayerID),
	%% 通知周围玩家- 玩家离开
	%% 地图模版MAP_EVENT_PLAYER_ENTER 事件
	NewModuleState = 
		case catch MapModule:on_trigger(?MAP_EVENT_PLAYER_LEAVE(PlayerID, Behaviour), MapID, MapDocID, ModuleState) of
            {ok, NewState} ->
				NewState;
			_ ->
				ModuleState
		end,
	{reply, ok, MapState#map_state{module_state = NewModuleState}};

%% 容错
handle_call(Request, _From, MapState = #map_state{map_id = MapID, module = MapModule}) ->
	?DEBUG("地图ID[~w] 地图模块[~w] handle call 不匹配, 消息:~w",[MapID, MapModule, Request]),
	{reply, {error, nomatch}, MapState}.

%% ==========  handle_cast/2 =====================
%% 地图透传消息
handle_cast({trigger, Msg}, MapState = #map_state{map_id = MapID, map_doc_id = MapDocID, module = MapModule, module_state = ModuleState}) ->
	NewModuleState = 
		case catch MapModule:on_trigger(Msg, MapID, MapDocID, ModuleState) of
            {ok, NewState} ->
				NewState;
			_ ->
				ModuleState
		end,
    {noreply, MapState#map_state{module_state = NewModuleState}};

%% 地图行走
handle_cast({move, _Data}, MapState) ->
    {noreply, MapState};

%% 容错
handle_cast(Msg, MapState = #map_state{map_id = MapID, module = MapModule}) ->
	?DEBUG("地图ID[~w] 地图模块[~w] handle cast 不匹配, 消息:~w",[MapID, MapModule, Msg]),
	{noreply, MapState}.

%% ==========  handle_info/2 =====================
%% 地图循环
handle_info(map_loop, MapState = #map_state{map_id = MapID, map_doc_id = MapDocID, module = MapModule, module_state = ModuleState}) ->
	erlang:send_after(?MAP_LOOP_TIME, self(),  map_loop),
	%% 怪物AI循环
	%% 地图模版逻辑循环
	NewModuleState =
		case catch MapModule:on_loop(MapID,  MapDocID, ModuleState) of
            {ok, NewState} ->
				NewState;
			_ ->
				ModuleState
		end,
	{noreply, MapState#map_state{module_state = NewModuleState}};

%% 容错
handle_info(Info, MapState = #map_state{map_id = MapID, module = MapModule}) ->
	?DEBUG("地图ID[~w] 地图模块[~w] handle info 不匹配, 消息:~w",[MapID, MapModule, Info]),
	{noreply, MapState}.

%% terminate/2
terminate(_Reason, #map_state{map_id = MapID, map_doc_id = MapDocID, module = MapModule, module_state = ModuleState}) ->
	?DEBUG("地图ID[~w] 地图模块[~w] 关闭",[MapID, MapModule]),
	try MapModule:on_stop(MapID, MapDocID, ModuleState)
	catch _:Error ->
			?ERROR("地图ID[~w] 地图模块[~w] on_stop 失败, 描述:~w",[MapID, MapModule, Error])
	end,
    ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

