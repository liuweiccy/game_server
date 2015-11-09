%% @author 503407245@qq.com
%% @doc 登录目录


-module(lib_login).

-include("common.hrl").
-include("player.hrl").
-include("ets.hrl").
-include("db_table.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([account_login/1, create_role/4]).

account_login(Account) ->
	?QUICK_IF_NOT(check_charlist(Account), throw({error, account_charlist})),
	case catch find_account(Account) of
		none ->
			{create, ?R_LOGIN_FIRST};
		{ok, PlayerID} ->
			%?IF(mod_player:is_alive(PlayerID), mod_player:stop(PlayerID), skip), %% 顶号处理
			{ok, PlayerID, ?R_LOGIN_OK};
		{error, Reason} ->
			{error, Reason, ?R_LOGIN_PARAM_ERROR};
		_Other ->
			{error, _Other, ?R_LOGIN_ERROR}
	end.

find_account(Account) ->
	case dets:lookup(?DB_ACCOUNT, Account) of
		[] -> % 第一次登录 无账号
			none;
		[#db_account{player_list = [PlayerID]}|_] ->
			{ok, PlayerID}
	end.

create_role(Account, Career, Sex, Name) ->
	?QUICK_IF_NOT(check_charlist(Account), throw({error, account_charlist})),
	%?QUICK_IF_NOT(check_charlist(Name), throw({error, name_charlist})),
	?QUICK_IF_NOT(Sex == 1 orelse Sex == 0, throw({error, sex})),
	?QUICK_IF_NOT(find_account(Account) == none, throw({error, account_error})),
    %% TODO 伪代码，真实需ID分配规则
    PlayerId = util:longunixtime(),
    DbAccount = #db_account{account = Account, player_list = [PlayerId]},
    DbPlayer = #db_player{id = PlayerId, name = Name, career = Career, sex = Sex},
    dets:insert(?DB_ACCOUNT, DbAccount),
    dets:insert(?DB_PLAYER, DbPlayer),
	{ok, PlayerId}.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_charlist(List) when is_binary(List) ->
	check_charlist(erlang:binary_to_list(List));
check_charlist([]) ->
    true;
check_charlist([H|T]) 
    when (H >= 65 andalso H =< 90) 
    orelse (H >= 97 andalso H =< 122)
    orelse (H >= 48 andalso H =< 57) ->
    check_charlist(T);
check_charlist(_L) ->
   	false.