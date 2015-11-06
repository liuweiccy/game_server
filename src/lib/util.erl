-module(util).

-export([
        unixtime/0,
        longunixtime/0,
		is_same_day/1,
		is_same_day/2,
		add_set/2,
        to_list/1,
        get_dict/2,
        set_dict/2,
        md5/1,
        rand/2,
	    ceil/1,
        floor/1,
        sleep/1,
        sleep/2,
		qsort/1,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        for/3,
        for/4,
        string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
        term_to_bitstring/1,
		register_name/2
    ]).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X)   -> float_to_list(X);
to_list(X) when is_atom(X)    -> atom_to_list(X);
to_list(X) when is_binary(X)  -> binary_to_list(X);
to_list(X) when is_list(X)    -> X.

get_dict(Key, Default) ->
    case erlang:get(Key) of
        undefined ->
            Default;
        Val ->
            Val
    end.
set_dict(Key, Val) ->
    erlang:put(Key, Val).

add_set(E, List) ->
	case lists:member(E, List) of
		true ->
			List;
		false ->
			[E|List]
	end.

%% =============================================
%% 取得当前的unix时间戳
%% Release版本取本机时间
%% Debug版本取本机时间+时间偏移量
%% 时间偏移量 = 后台设置时间 - 当前时间
%% =============================================
-ifdef(DEBUG).
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S + mochiglobal:get(time_offset, 0).

longunixtime() ->
    {M, S, Ms} = os:timestamp(),
    M * 1000000000 + S*1000 + Ms div 1000 + mochiglobal:get(time_offset, 0) * 1000.

-else.
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

longunixtime() ->
    {M, S, Ms} = os:timestamp(),
    M * 1000000000 + S*1000 + Ms div 1000.

-endif.


is_same_day(T) ->
	is_same_day(unixtime(), T).

is_same_day(T1, T2) ->
	{D1, _} = calendar:seconds_to_daystime(T1),
	{D2, _} = calendar:seconds_to_daystime(T2),
	D1 == D2.

%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) ->
    random:uniform(Max - Min) + Min.

%%向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

sleep(T) ->
    receive
    after T -> ok
    end.

sleep(T, F) ->
    receive
    after T -> F()
    end.

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

register_name(Term, Key) when erlang:is_atom(Term) andalso erlang:is_integer(Key) ->
	string_to_term(term_to_string(Term) ++ erlang:integer_to_list(Key)).


% 根据Privot 分割列表, {Smaller, Bigger}
spilt(Privot,L)->
	spilt(Privot,L,[],[]).

spilt(_Privot,[],Smaller,Bigger)->
	{Smaller,Bigger};

spilt(Privot,[H|T],Smaller,Bigger) when H<Privot->
	spilt(Privot,T,[H|Smaller],Bigger);

spilt(Privot,[H|T],Smaller,Bigger) when H>=Privot->
	spilt(Privot,T,Smaller,[H|Bigger]).

% 快速排序
qsort(X)->
	qsort(X,[]).
qsort([], Tail) -> Tail;
qsort([Pivot|Rest],Tail)->
	{Smaller,Bigger}=spilt(Pivot,Rest),
	qsort(Smaller,[Pivot|qsort(Bigger,Tail)]).
