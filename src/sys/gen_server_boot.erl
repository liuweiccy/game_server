%%--------------------------------------------
%% @author jiangxiaowei
%%--------------------------------------------

-module(gen_server_boot).
-behaviour(gen_server).

-include("common.hrl").

-export([start_link/4, start_link/3, init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(boot_state,{mod,state}).
%%---------------------------------------------------------
%% gen_server
%%---------------------------------------------------------
start_link(ServerName, Module, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, [Module,Args], Options).

start_link(Module, Args, Options) ->
	gen_server:start_link(?MODULE, [Module,Args], Options).

init([Module,Args]) ->
    case catch Module:init(Args) of
		{ok, State} ->
    		{ok,#boot_state{mod = Module,state = State}};
		Reason ->
			{stop, Reason}
	end.

handle_call(Request, From, BootState = #boot_state{mod = Module,state = State}) ->
    case catch Module:handle_call(Request,From,State) of
        {reply,Reply,NewState} ->
            {reply,Reply,BootState#boot_state{state = NewState}};
        {reply,Reply,NewState,hibernate} ->
            {reply,Reply,BootState#boot_state{state = NewState},hibernate};
        {reply,Reply,NewState,Timeout} ->
            {reply,Reply,BootState#boot_state{state = NewState},Timeout};
        {stop,Reason,Reply,NewState} ->
            {stop,Reason,Reply,BootState#boot_state{state = NewState}};
        {stop,Reason,NewState} ->
            {stop,Reason,BootState#boot_state{state = NewState}};
        Other ->
            ?ERROR("Module:~w ,State:~w ,handle_call error ~w",[Module,State,Other]),
            {reply,error,BootState}
    end.

handle_cast(Request, BootState = #boot_state{mod = Module,state = State}) ->
    case catch Module:handle_cast(Request,State) of
        {noreply,NewState} ->
            {noreply,BootState#boot_state{state = NewState}};
        {noreply,NewState,hibernate} ->
            {noreply,BootState#boot_state{state = NewState},hibernate};
        {noreply,NewState,Timeout} ->
            {noreply,BootState#boot_state{state = NewState},Timeout};
        {stop,Reason,NewState} ->
            {stop,Reason,BootState#boot_state{state = NewState}};
        Other ->
            ?ERROR("Module:~w ,State:~w ,handle_cast error ~w",[Module,State,Other]),
            {noreply,BootState}
    end.

handle_info(Request, BootState = #boot_state{mod = Module,state = State}) ->
    case catch Module:handle_info(Request,State) of
        {noreply,NewState} ->
            {noreply,BootState#boot_state{state = NewState}};
        {noreply,NewState,hibernate} ->
            {noreply,BootState#boot_state{state = NewState},hibernate};
        {noreply,NewState,Timeout} ->
            {noreply,BootState#boot_state{state = NewState},Timeout};
        {stop,Reason,NewState} ->
            {stop,Reason,BootState#boot_state{state = NewState}};
        Other ->
            ?ERROR("Module:~w ,State:~w ,handle_info error ~w",[Module,State,Other]),
            {noreply,BootState}
    end.

terminate(Reason, #boot_state{mod = Module,state = State}) ->
    try Module:terminate(Reason,State)
    catch Error:ErrorReason ->
        ?ERROR("Module:~w ,State:~w ,terminate error ~w",[Module,State,{Error,ErrorReason}]),
        ok
    end.
        
code_change(OldVsn, BootState = #boot_state{mod = Module,state = State}, Extra) ->
    case catch Module:code_change(OldVsn,State,Extra) of
        {ok,NewState} ->
            {ok,BootState#boot_state{state = NewState}};
        {error,Reason} ->
            {error,Reason};
        Other ->
            ?ERROR("Module:~w ,State:~w ,code_change error ~w",[Module,State,Other]), 
            {error,callback}
    end.
        
        
        

