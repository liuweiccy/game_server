%% @author jiangxiaowei
%% @doc 玩家进程.


-module(proc_player).
-behaviour(gen_server).

-include("common.hrl").
-include("player.hrl").
-include("map.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BINARY_TIMEOUT, 1000). % 解析协议超时时间
-define(HEART_TIMEOUT, 30000). % 心跳包超时时间
-define(HEADER_LENGTH, 2). % 消息头长度

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, stop/1]).

-export([async_run/2, sync_apply/2]).

-export([is_alive/1]).

-type opts() :: []. % 暂无控制参数
-spec start_link(ranch:ref(), inet:socket(), module(), opts()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    gen_server_boot:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

stop(PlayerID) when erlang:is_integer(PlayerID) ->
	stop(util:register_name(player, PlayerID));
stop(RegName) when erlang:is_atom(RegName) ->
	case is_alive(RegName) of
		true ->
			gen_server:cast(RegName, stop);
		false ->
			skip
	end.

is_alive(RegName) when erlang:is_atom(RegName) ->
    erlang:whereis(RegName) =/= undefined;
is_alive(PlayerID) when erlang:is_integer(PlayerID)->
    erlang:whereis(util:register_name(player, PlayerID)) =/= undefined.

async_run(PlayerID, F) ->
    gen_server:cast(util:register_name(player, PlayerID), {async_run, F}).
sync_apply(PlayerID, F) ->
    ?CALL(util:register_name(player, PlayerID), {sync_apply, F}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
init([_Ref, Socket, _Transport, _Opts]) ->
	erlang:process_flag(trap_exit, true),
    lib_player:set_socket(Socket),
	{ok, {Addr, Port}} = inet:peername(Socket),
	?DEBUG("客户端进程启动, IP:~w, 端口:~w",[Addr, Port]),
	{ok, #player_state{socket = Socket}}.

%% handle_call/3
handle_call({sync_apply, F}, _From, PlayerStatus = #player_state{login_step = ?P_LOGIN_STATUS_GAME}) ->
	case catch F(PlayerStatus) of
		{ok, Reply} ->
			{reply, Reply, PlayerStatus};
		{ok, Reply, NewPlayerStatus} ->
			{reply, Reply, NewPlayerStatus};
		_Reason ->
            ?ERROR("[~w]的玩家进程处理sync_apply消息时出错: [Reason:~w]", [lib_player:get_id(PlayerStatus), _Reason]),
			{reply, error, PlayerStatus}
	end;

handle_call(_Request, _From, State) ->
	?WARNING("[mod_player]进程 handle_call 不匹配, Request:~w, From:~w, State:~w",[_Request, _From, State]),
    {reply, ok, State}.


%% handle_cast/2
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast({async_run, F}, PlayerStatus = #player_state{login_step = ?P_LOGIN_STATUS_GAME}) ->
	case catch F(PlayerStatus) of
		ok ->
			{noreply, PlayerStatus};
		{ok, NewPlayerStatus} when is_record(NewPlayerStatus,player_state)->
			{noreply, NewPlayerStatus};
		_Reason ->
            ?ERROR("[~w]的玩家进程处理async_run消息时出错: [Reason:~w]", [lib_player:get_id(PlayerStatus), _Reason]),
			{noreply, PlayerStatus}
	end;	

handle_cast({send, BinData} , PlayerStatus = #player_state{player_id = PlayerID, login_step = ?P_LOGIN_STATUS_GAME, socket = Socket}) ->
	try gen_tcp:send(Socket, BinData)
	catch _:Reason -> ?WARNING("玩家[~w]发送协议错误, 描述:~w",[PlayerID, Reason]) end,
	{noreply, PlayerStatus};

%% handle_cast({player_battle_result, BattleResult}, PlayerStatus = #player_state{login_step = ?P_LOGIN_STATUS_GAME})->
%% 	case catch player_battle_result:process(BattleResult, PlayerStatus) of
%% 		{ok, NewPlayerStatus} when is_record(NewPlayerStatus,player_state)->
%% 			{noreply, NewPlayerStatus};
%% 		_Reason ->
%%             ?ERROR("[~w]的玩家进程处理player_battle_result消息时出错: [Reason:~w]", [lib_player:get_id(PlayerStatus), _Reason]),
%% 			{noreply, PlayerStatus}
%% 	end;
%%
%% handle_cast({pet_battle_result, BattleResult}, PlayerStatus = #player_state{login_step = ?P_LOGIN_STATUS_GAME})->
%% 	case catch pet_battle_result:process(BattleResult, PlayerStatus) of
%% 		{ok, NewPlayerStatus} when is_record(NewPlayerStatus,player_state)->
%% 			{noreply, NewPlayerStatus};
%% 		_Reason ->
%%             ?ERROR("[~w]的玩家进程处理pet_battle_result消息时出错: [Reason:~w]", [lib_player:get_id(PlayerStatus), _Reason]),
%% 			{noreply, PlayerStatus}
%% 	end;

handle_cast(_Msg, State) ->
	?WARNING("[mod_player]进程 handle_cast 不匹配, Msg:~w, State:~w",[_Msg, State]),
    {noreply, State}.


%% handle_info/2
%% 玩家进程 循环
handle_info(player_loop , PlayerStatus) ->
	erlang:send_after(?PLAYER_LOOP_TIME, self(), player_loop),
	{noreply, PlayerStatus};

%% 网络启动
handle_info({shoot, _Ref, _Transport, Socket, _AckTimeout}, PlayerStatus) ->
    {ok, {Addr, Port}} = inet:peername(Socket),
    ?DEBUG("开始接受客户端:~w 网络协议",[{Addr, Port}]),
    async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    {noreply, PlayerStatus};

%% 收到包头
handle_info({inet_async, Socket, _Ref, {ok, <<PacketLen:16>>}}, PlayerStatus = #player_state{socket = Socket}) ->
	case PacketLen > 1024 of
		true ->
			?ERROR("收到异常网络包, 中断连接."),
			{stop, normal, PlayerStatus};
		false ->
			async_recv(Socket, PacketLen, ?BINARY_TIMEOUT),
			{noreply, PlayerStatus}
	end;
%% 收到包体
handle_info({inet_async, Socket, _Ref, {ok, <<Cmd:32, BinData/binary>>}}, PlayerStatus = #player_state{socket = Socket}) ->
	async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	case catch handle_protocol(Cmd, BinData, PlayerStatus) of
		ok ->
			{noreply, PlayerStatus};
		{ok, NewPlayerStatus} ->
			{noreply, NewPlayerStatus};
		{stop, _Reason, NewPlayerStatus} ->
			?DEBUG("协议处理stop进程,  描述:~w",[_Reason]),
			{stop, normal, NewPlayerStatus};
		_Other ->
			?DEBUG("协议处理异常, 描述:~w",[_Other]),
			{stop, normal, PlayerStatus}
	end;

%% Socket错误
handle_info({inet_async, Socket, _Ref, Reason}, State = #player_state{socket = Socket}) ->
	?DEBUG("Socket 错误, 关闭进程 , 描述:~w",[Reason]),
	{stop, normal, State};

handle_info(_Info, State) ->
	?WARNING("[mod_player]进程 handle_info 不匹配, Info:~w, State:~w",[_Info, State]),
    {noreply, State}.


%% terminate/2
terminate(_Reason, State = #player_state{socket = Socket}) ->
	{ok, {Addr, Port}} = inet:peername(Socket),
	?DEBUG("客户端进程退出, IP:~w, 端口:~w",[Addr, Port]),
	do_logout(State),
	gen_tcp:close(Socket),
    ok.

do_logout(#player_state{player_id = PlayerID} = PlayerStatus) when PlayerID > 0 ->
	?DEBUG("玩家[~w] 进程退出",[PlayerID]),
	lib_player:leave_map(PlayerStatus, ?BEHAVIOR_LOGOUT), % 离场通知玩家广播
	lib_online:del_player(PlayerID), % 删除ETS_MAP_PLAYER表中数据
	lib_player:on_save(PlayerStatus), % 玩家数据存储
	ok;
do_logout(_) ->
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

handle_protocol(Cmd, BinData, PlayerStatus = #player_state{login_step = LoginStep}) ->
    case protobuff_routing:decode(Cmd, BinData) of
        {handle_account, Data} when LoginStep < ?P_LOGIN_STATUS_GAME ->
            handle_account:handle(Data, PlayerStatus);
        {HandleModule, Data} when LoginStep == ?P_LOGIN_STATUS_GAME ->
            HandleModule:handle(Data, PlayerStatus);
        _Other ->
            {stop, illegal_porotocol, PlayerStatus}
    end.
