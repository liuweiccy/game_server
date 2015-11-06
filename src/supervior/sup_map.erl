%% @author dzlaozhu35@outlook.com
%% @doc 地图监控树


-module(sup_map).
-behaviour(supervisor).
-export([init/1]).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/0, start_child/1]).

start(SupPid) ->
	 {ok,_} = supervisor:start_child(SupPid, ?SUPERVIOR_CHILD_SPEC(?MODULE, [])),
	 ?INFO("start map supervisor"),
	 ok.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child([MapID, MapDocID, MapModule]) ->
	supervisor:start_child(?MODULE, [MapID, MapDocID, MapModule]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
init([]) ->
    {ok,{{simple_one_for_one,10,10}, [{proc_map,{proc_map,start_link,[]},
	      transient,2000,worker,[proc_map]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


