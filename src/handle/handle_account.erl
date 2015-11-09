%% @author 503407245@qq.com
%% @doc  account协议处理

-module(handle_account).

-include("common.hrl").
-include("player.hrl").
-include("account_pb.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/2]).

%% 处理登录
handle(#account_login_tos{account = Account}, PlayerState = #player_state{socket = Socket, login_step = ?P_LOGIN_STATUS_NORMAL}) ->
	case catch lib_login:account_login(Account) of
		{ok, PlayerID, Code} ->
			{ok, BinData} = protobuff_routing:encode(#account_login_toc{code = Code}),
			gen_tcp:send(Socket, BinData),
			{ok, PlayerState#player_state{player_id = PlayerID, login_step = ?P_LOGIN_STATUS_LOGIN}};
		{create, Code} ->
			{ok, BinData} = protobuff_routing:encode(#account_login_toc{code = Code}),
			gen_tcp:send(Socket, BinData),
			{ok, PlayerState#player_state{login_step = ?P_LOGIN_STATUS_CREATE}};
		{error, Reason, Code} ->
			?WARNING("帐号登录失败, 描述:[~w]",[Reason]),
			{ok, BinData} = protobuff_routing:encode(#account_login_toc{code = Code}),
			gen_tcp:send(Socket, BinData),
			{stop, login_fail};
		Error ->
			?ERROR("帐号登录异常, 错误:[~w]",[Error]),
			{stop, login_error}
	end;

%% 处理创建角色
handle(#create_role_tos{account = Account, career = Facelook, sex = Sex, name = Name},
       PlayerState = #player_state{socket = Socket, login_step = ?P_LOGIN_STATUS_CREATE}) ->
	case catch lib_login:create_role(Account, Facelook, Sex, Name) of
		{ok, PlayerID}  ->
			{ok, BinData} = protobuff_routing:encode(#create_role_toc{code = 0}),
			gen_tcp:send(Socket, BinData),
			{ok, PlayerState#player_state{player_id = PlayerID, login_step = ?P_LOGIN_STATUS_LOGIN}};
		{error, Reason} ->
			?WARNING("创建角色失败, 描述:[~w]",[Reason]),
			{ok, BinData} = protobuff_routing:encode(#create_role_toc{code = 1}),
			gen_tcp:send(Socket, BinData),
			{stop, create_role_fail};
		_Other ->
			?ERROR("创建角色异常, 错误:[~w]",[_Other]),
			{stop, create_role_error}
	end;

%% 处理进入游戏
handle(#select_login_tos{},  #player_state{player_id = PlayerID, login_step = ?P_LOGIN_STATUS_LOGIN}) when PlayerID > 0 ->
	case catch lib_player:on_enter(PlayerID) of
		{ok, PlayerStatus} ->
			%% 此处发送客户端所需的数据协议
			{ok, PlayerStatus#player_state{login_step = ?P_LOGIN_STATUS_GAME}};
		_Other ->
			?ERROR("进入游戏异常, 错误:[~w]",[_Other]),
			{stop, enter_game_error}
	end;

%% 帐号登录模块的协议badmatch直接断开连接
handle(Data, PlayerStatus) ->
	?ERROR("[~w] handle nomatch, PlayerID:~w, Cmd:~w, Data:~w",[lib_player:get_id(PlayerStatus), Data]),
	{stop,  badmatch}.

%% ====================================================================
%% Internal functions
%% ====================================================================


