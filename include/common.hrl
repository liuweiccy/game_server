%%-include_lib("stdlib/include/ms_transform.hrl").

%% 带catch的gen_server:call/2，返回{error, timeout} | {error, noproc} | {error, term()} | term() | {exit, normal}
%% case catch gen_server:call(Pid, Request)
-define(CALL(Pid, Request),
    case catch gen_server:call(Pid, Request) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', normal} -> {exit, normal};
        {'EXIT', Msg} -> {error, Msg};
        Rtn -> Rtn
    end
).

-define(TRUE, 1).
-define(FALSE, 0).

-define(R_LOGIN_OK, 0).
-define(R_LOGIN_FIRST, 1).
-define(R_LOGIN_ALREADY_LOGIN,2).
-define(R_LOGIN_PARAM_ERROR,3).
-define(R_LOGIN_ERROR,4).

-define(IF(Condition,Expression1,Expression2), 
    case (Condition) of
        true -> Expression1;
        false -> Expression2
    end
).

%% 位操作
-define(SET_BIT(_Index, _Number), (1 bsl _Index bor _Number)).
-define(CHECK_BIT(_Index, _Number), ((_Number bsr _Index) band 1 == 1)).
-define(CLEAR_BIT(_Index, _Number), ?IF(?CHECK_BIT(_Index, _Number), ((1 bsl _Index)  bxor _Number), _Number)).

-define(HANDLE_CONTINUE, ok).
-define(HANDLE_NEW_STATE(NewState), {ok, NewState}).

-define(QUICK_IF(Condition,Msg), ((Condition) andalso throw(Msg))).
-define(QUICK_IF_NOT(Condition,Msg), ((Condition) orelse throw(Msg))).

-define(ONE_THOUSAND, 1000).
-define(ONE_MILLION, 1000000).
-define(TEN_MILLION, ?ONE_MILLION * 10).
-define(ONE_HOUR_SECOND, 60 * 60).
-define(ONE_DAY_SECOND, 24 * 60 * 60).

%% 进程闭包函数请求
-define(INVOKER_ASYNC(F), {async, F}).
-define(INVOKER_SYNC(F), {sync, F}).

-define(SUPERVIOR_CHILD_SPEC(Module, Params), {Module, {Module, start_link,Params}, transient, infinity, supervisor, [Module]}).

-define(NONE, none).

-define(CH2B(Chars), unicode:characters_to_list(Chars)).

%% 日志相关
-define(PRINT(Format, Args),
    io:format(?CH2B(Format), Args)).

-define(DEBUG(Format),
    logger:debug_msg(?MODULE,?LINE,?CH2B(Format), [])).
-define(DEBUG(Format, Args),
    logger:debug_msg(?MODULE,?LINE,?CH2B(Format), Args)).

-define(INFO(Format),
    logger:info_msg(?MODULE,?LINE,?CH2B(Format), [])).
-define(INFO(Format, Args),
    logger:info_msg(?MODULE,?LINE,?CH2B(Format), Args)).
		
-define(WARNING(Format),
    logger:warning_msg(?MODULE,?LINE,?CH2B(Format), [])).
-define(WARNING(Format, Args),
    logger:warning_msg(?MODULE,?LINE,?CH2B(Format), Args)).
	
-define(ERROR(Format),
    logger:error_msg(?MODULE,?LINE,?CH2B(Format), [])).
-define(ERROR(Format, Args),
    logger:error_msg(?MODULE,?LINE,?CH2B(Format), Args)).

-define(CTL(Format),
    logger:critical_msg(?MODULE,?LINE,?CH2B(Format), [])).
-define(CTL(Format, Args),
    logger:critical_msg(?MODULE,?LINE,?CH2B(Format), Args)).


%% 服务器PRC 返回状态
-define(RPC_STATUS_SUCCESS, 					0).	
-define(RPC_STATUS_NORUN, 					    1).
-define(RPC_STATUS_USAGE, 						2).			
-define(RPC_STATUS_BADRPC, 					    3).
-define(RPC_STATUS_ERROR, 						4).	
-define(RPC_STATUS_STARTING, 				    5).
-define(RPC_STATUS_RUNNING, 				    6).
-define(RPC_STATUS_STOPING, 					7).
