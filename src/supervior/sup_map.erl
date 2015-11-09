%% @author 503407245@qq.com
%% @doc 地图监控树


-module(sup_map).
-behaviour(supervisor).
-export([init/1]).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/0, start_child/1, terminate_childs/0]).

start(SupPid) ->
	 {ok,_} = supervisor:start_child(SupPid, ?SUPERVIOR_CHILD_SPEC(?MODULE, [])),
	 ?INFO("start map supervisor"),
	 ok.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child([MapID, MapDocID, MapModule]) ->
	supervisor:start_child(?MODULE, [MapID, MapDocID, MapModule]).

terminate_childs() ->
    lists:foreach(
        fun ({_, Pid, _, _}) ->
            case catch gen_server:call(Pid, stop) of
                ok ->
                    ok;
                Other ->
                    ?ERROR("stop child:~p error:~p",[erlang:process_info(Pid, registered_name), Other])
            end
        end, supervisor:which_children(?MODULE)).

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


