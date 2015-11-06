-module(admin).

-export([reload/1, reload/0]).

reload() ->
    Modules = all_changed(),
    reload(Modules).

reload(Modules) ->
    [begin code:purge(M), code:load_file(M) end || M <- Modules].


all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn),is_changed(M)].
	
is_changed(M) ->
    try module_vsn(M:module_info()) =/=
		module_vsn(code:get_object_code(M))
    catch
		_:_ -> false
    end.

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam), Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

