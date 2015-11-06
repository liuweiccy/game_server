%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2015 下午 11:06
%%%-------------------------------------------------------------------
-module(game).
-author("Administrator").

%% API
-export([start/0]).

start() ->
    ok = application:start(?MODULE).

