%%--------------------------------------------
%% @author 503407245@qq.com
%%--------------------------------------------

-module(gen_server_boot).
-behaviour(gen_server).

-include("common.hrl").

-export([start_link/4, start_link/3, init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([invoke_async/2, invoke_sync/2]).

-record(boot_state,{mod,state}).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
term()),
                    State :: term()) ->
                       term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
                         {ok, NewState :: term()} | {error, Reason :: term()}.

%%---------------------------------------------------------
%% API
%%---------------------------------------------------------

invoke_async(Pid, F) ->
    gen_server:cast(Pid, ?INVOKE_ASYNC(F)).
invoke_sync(Pid, F) ->
    ?CALL(Pid, ?INVOKE_SYNC(F)).

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

handle_call(stop, _From, BootState) ->
    {stop, normal, ok, BootState};
handle_call(?INVOKE_SYNC(F), _From, BootState = #boot_state{state = State}) ->
    case catch F(State) of
        {ok, Reply} ->
            {reply, Reply, BootState};
        {ok, Reply, NewState} ->
            {reply, Reply, BootState#boot_state{state = NewState}};
        _Reason ->
            ?ERROR("proc invoke_sync error, Reason:~w", [_Reason]),
            {reply, error, BootState}
    end;
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

handle_cast(stop, BootState) ->
    {stop, normal, BootState};
handle_cast(?INVOKE_ASYNC(F), BootState = #boot_state{state = State}) ->
    case catch F(State) of
        ok ->
            {noreply, BootState};
        {ok, State} ->
            {noreply, BootState#boot_state{state = State}};
        _Reason ->
            ?ERROR("proc invoke_async error: [Reason:~w]", [_Reason]),
            {noreply, BootState}
    end;
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
        
        
        

