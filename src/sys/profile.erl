-module(profile).

%% API
-export([
  	eprof_start/0, eprof_stop/0,
  	fprof_start/0, fprof_start/1,fprof_stop/0, 
  	queue/1, memory/1,reds/1
]).

-define(APPS, [server]).

%%====================================================================
%% API
%%====================================================================

eprof_start() ->
  	eprof:start(),
  	case lists:keyfind(running, 1, application:info()) of
    	{_, Apps} ->
      		case get_procs(?APPS, Apps) of
        		[] ->
          			{error, no_procs_found};
        		Procs ->
          			eprof:start_profiling(Procs)
      		end;
    	_ ->
      		{error, no_app_info}
  	end.

fprof_start() ->
  	fprof_start(0).

fprof_start(Duration) ->
  	case lists:keyfind(running, 1, application:info()) of
    	{_, Apps} ->
      		case get_procs(?APPS, Apps) of
        		[] ->
          			{error, no_procs_found};
        		Procs ->
          			fprof:trace([start, {procs, Procs}]),
          			io:format("Profiling started~n"),
          			case  Duration > 0 of
						true ->
	            			timer:sleep(Duration*1000),
	            			fprof_stop();
            			false->
              				ok
          			end
      		end;
   		 _ ->
      		{error, no_app_info}
  	end.

fprof_stop() ->
  	fprof:trace([stop]),
  	fprof:profile(),
  	fprof:analyse([totals, no_details, {sort, own}, no_callers, {dest, "fprof.analysis"}]),
  	fprof:stop(),
  	format_fprof_analyze().

eprof_stop() ->
  	eprof:stop_profiling(),
%%   	eprof:log("eprof_procs.analysis"),
%%   	eprof:analyze(procs, [{sort, calls}]),
	eprof:log("eprof_total.analysis"),
	eprof:analyze(total, [{sort, calls},{filter, [{calls, 1000}]}]),
  	eprof:stop().

queue(N) ->
  	dump(N, lists:reverse(lists:ukeysort(1, all_pids(queue)))).

memory(N) ->
  	dump(N, lists:reverse(lists:ukeysort(3, all_pids(memory)))).

reds(N) ->
  	dump(N, lists:reverse(lists:ukeysort(4, all_pids(reductions)))).

%%====================================================================
%% Internal functions
%%====================================================================
get_procs(Apps, AppList) ->
  	io:format("Searching for processes to profile...~n", []),
  	Leaders = lists:foldl(
    	fun
		 	({App, Leader},Acc)->
         		case lists:member(App, Apps) of
            		true ->
                		[Leader|Acc];
        	 		false ->
                  		Acc
          		end;
      		(_,Acc) ->
        		Acc
    	end,[], AppList),
  	get_procs2(Leaders).

get_procs2(Leaders) ->
  	lists:filter(
    	fun(Pid) ->
    		case process_info(Pid, group_leader) of
	    		{_, Leader} ->
					lists:member(Leader, Leaders);
	      		_ ->
	          		false
			end
    	end, processes()).

format_fprof_analyze() ->
    case file:consult("fprof.analysis") of
    	{ok, [_, [{totals, _, _, TotalOWN}] | Rest]} ->
      	    OWNs =
    		lists:flatmap(
	    		fun({MFA, _, _, OWN}) ->
	     	   		Percent = OWN*100/TotalOWN,
	      			case round(Percent) of
					0 -> [];
	        		_ -> [{mfa_to_list(MFA), Percent}]
	      			end
	    		end, Rest),
		    ACCs = collect_accs(Rest),
		    MaxACC = find_max(ACCs),
		    MaxOWN = find_max(OWNs),
		    io:format("=== Sorted by OWN:~n"),
      		lists:foreach(
		        fun({MFA, Per}) ->
		            L = length(MFA),
		            S = lists:duplicate(MaxOWN - L + 2, $ ),
		            io:format("~s~s~.2f%~n", [MFA, S, Per])
		        end, lists:reverse(lists:keysort(2, OWNs))),
      		io:format("~n=== Sorted by ACC:~n"),
	        lists:foreach(
		        fun({MFA, Per}) ->
		            L = length(MFA),
		            S = lists:duplicate(MaxACC - L + 2, $ ),
		            io:format("~s~s~.2f%~n", [MFA, S, Per])
		        end, lists:reverse(lists:keysort(2, ACCs)));
    	Err ->
      		Err
  end.

mfa_to_list({M, F, A}) ->
  	atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
mfa_to_list(F) when is_atom(F) ->
  	atom_to_list(F).

find_max(List) ->
  	find_max(List, 0).

find_max([{V, _}|Tail], Acc) ->
  	find_max(Tail, lists:max([length(V), Acc]));
find_max([], Acc) ->
  	Acc.

collect_accs(List) ->
  	List1 = lists:filter(
	    fun
			({{sys, _, _}, _, _, _}) ->
	      		false;
	      	({suspend,_,_,_}) ->
	        	false;
	      	({{gen_fsm, _, _},_,_,_}) ->
	        	false;
	      	({{gen, _, _},_,_,_}) ->
	        	false;
	      	({{gen_server, _, _},_,_,_}) ->
	        	false;
	      	({{proc_lib, _, _},_,_,_}) ->
	        	false;
	      	(_) ->
	        	true
	    end, List),
	calculate(List1).

calculate(List1) ->
  	TotalACC = lists:sum([A || {_, _, A, _} <- List1]),
  	List2 = lists:foldl(
		fun({MFA, _, ACC, _},NewList) ->
    	Percent = ACC*100/TotalACC,
			case round(Percent) of
			  	0 -> NewList;
		      	_ -> [{mfa_to_list(MFA), Percent}|NewList]
		    end
   		end,[],List1),
  	lists:reverse(List2).

all_pids(Type) ->
  	lists:foldl(
    	fun
			(P, Acc) when P == self() ->
      	   		Acc;
      		(P, Acc) ->
        		case catch process_info(P,[message_queue_len, memory, reductions, dictionary, current_function, registered_name]) of
			    	[{_, Len}, {_, Memory}, {_, Reds}, {_, Dict}, {_, CurFun}, {_, RegName}] ->
			        	IntQLen = get_internal_queue_len(Dict),
			            case Type == queue andalso Len == 0 andalso IntQLen == 0 of
							true -> Acc;
			              	false  ->[{lists:max([Len, IntQLen]),Len,Memory, Reds, Dict, CurFun, P, RegName}|Acc]
			            end;
			     	_ ->
			            Acc
			   	end
    	end, [], processes()).

get_internal_queue_len(Dict) ->
  	case lists:keysearch('$internal_queue_len', 1, Dict) of
    	{value, {_, N}} -> N;
    	_ -> 0
  	end.

dump(N, Rs) ->
  	lists:foreach(
    	fun({_, MsgQLen, Memory, Reds, _Dict, CurFun, Pid, RegName}) ->
	      	io:format(
			  "*************************************************************~n"
			  "** pid(~s)~n"
		      "** registered name: ~p~n"
		      "** memory: ~p~n"
		      "** reductions: ~p~n"
		      "** message queue len: ~p~n"
		      "** current_function: ~p~n"
		      %"** dictionary: ~p~n"
			  "*************************************************************~n~n~n",
	          [pid_to_list(Pid), RegName, Memory, Reds, MsgQLen, CurFun])
    	end, lists:sublist(Rs,N)).
