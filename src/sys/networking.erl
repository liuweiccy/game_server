-module(networking).

-include("common.hrl").

-export([start/1, stop/0]).

-export([get_protocol_options/0, set_protocol_options/1]).

-export([get_max_connections/0, set_max_connections/1]).

%%开启tcp listener监控树
start(SupPid) ->
    {ok,_} = supervisor:start_child(SupPid, ?SUPERVIOR_CHILD_SPEC(ranch_sup, [])),
	{ok, Port} = application:get_env(listen_port),
	{ok, AcceptorCount} = application:get_env(tcp_acceptor_num),
    {ok, ProtoOptions} = application:get_env(protocol_options), % 网络进程参数
    {ok, SocketOptions} = application:get_env(tcp_options),
    {ok, MaxConnCounts} = application:get_env(max_connections),
    {ok, _} = ranch:start_listener(tcp, AcceptorCount, ranch_tcp,
                                   [{port, Port}, {max_connections, MaxConnCounts}|SocketOptions],
                                   proc_player, ProtoOptions),
    ?INFO("start listener[~w]",[Port]),
    ok.

stop() ->
    ranch:stop_listener(tcp).

get_protocol_options() ->
    ranch:get_protocol_options(tcp).

set_protocol_options(Options) ->
    ranch:set_protocol_options(tcp, Options).

get_max_connections() ->
    ranch:get_max_connections(tcp).

set_max_connections(Count) ->
    ranch:set_max_connections(tcp, Count).