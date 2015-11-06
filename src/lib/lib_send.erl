%% @author jiangxiaowei
%% @doc 协议发送模块.


-module(lib_send).

-include("common.hrl").
-include("player.hrl").
-include("ets.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([send_to_player/1, send_to_player/2, send_to_all/1]).

send_to_player(Protocol) ->
	Socket = lib_player:get_socket(),
    send_protocol(Socket, Protocol).
send_to_player(PlayerID, Protocol) ->
	gen_server:cast(util:register_name(player, PlayerID), {send, Protocol}).

send_to_all(BinData) ->
	MS = ets:fun2ms(fun(#player_online{player_id = ID}) -> ID  end),
	Matchs = ets:select(?ETS_PLAYER_ONLINE, MS),
	[send_to_player(PlayerID, BinData) || PlayerID <- Matchs].

%% ====================================================================
%% Internal functions
%% ====================================================================
send_protocol(Socket, BinData) ->
    catch erlang:port_command(Socket, BinData, [force]).

