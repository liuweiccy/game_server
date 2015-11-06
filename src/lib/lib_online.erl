%% @author ASUS
%% @doc @todo Add description to lib_online.


-module(lib_online).
-include("common.hrl").
-include("player.hrl").
-include("ets.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_player/1, del_player/1, add_player/1, update_element/2, count/0]).

del_player(PlayerID) ->
	ets:delete(?ETS_PLAYER_ONLINE, PlayerID).

get_player(PlayerID) ->
	case ets:lookup(?ETS_PLAYER_ONLINE, PlayerID) of
		[] ->
			?NONE;
		[OnlinePlayer|_] ->
            OnlinePlayer
	end.

add_player(PlayerState) when erlang:is_record(PlayerState, player_state) ->
    add_player(convert_player_online(PlayerState));
add_player(OnlinePlayer) when erlang:is_record(OnlinePlayer, player_online) ->
	ets:insert(?ETS_PLAYER_ONLINE, OnlinePlayer).

update_element(PlayerID, UpdateOpts) when erlang:is_integer(PlayerID) ->
	ets:update_element(?ETS_PLAYER_ONLINE, PlayerID, UpdateOpts).

count() ->
	ets:info(?ETS_PLAYER_ONLINE, size).

%% ====================================================================
%% Internal functions
%% ====================================================================
convert_player_online(#player_state{player_id = PlayerID}) ->
    #player_base_info{name = Name, career = Career, sex = Sex} = lib_player:get_base_info(),
    #player_level_info{level = Level} = lib_player:get_level_info(),
    #player_map_info{map_id = MapId} = lib_player:get_map_info(),
	#player_online{
				player_id = PlayerID,
				name = Name,
				sex = Sex,
				level = Level,
				career = Career,
				map_id = MapId}.

