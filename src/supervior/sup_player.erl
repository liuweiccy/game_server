%% @author dzlaozhu35@outlook.com
%% @doc 客户端连接监控树

-module(sup_player).

-behaviour(supervisor).

-include("common.hrl").

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/0, start_child/1]).

start(SupPid) ->
    {ok,_} = supervisor:start_child(SupPid, ?SUPERVIOR_CHILD_SPEC(?MODULE, [])),
	 ?INFO("start player supervisor"),
	 ok.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Socket) ->
	supervisor:start_child(?MODULE, [Socket]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
init([]) ->
    {ok,{{simple_one_for_one,10,10}, [{proc_player,{proc_player,start_link,[]},
	      transient,2000,worker,[proc_player]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


