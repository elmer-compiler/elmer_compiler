-module(elmer_util).

-export([ btoa/1
        , var/1
        , nvars/1
        , elm_module_to_atom/2
        ]).

elm_module_to_atom(Module, Package) when is_list(Module) andalso is_binary(Package) ->
    Mod = string:join(lists:map(fun binary_to_list/1, Module), "."),
    list_to_atom(lists:concat([ "Elm.", Mod ])).

var(Name) ->
    list_to_atom(lists:concat(["V_", binary_to_list(Name)])).

btoa(Binary) ->
    list_to_atom(binary_to_list(Binary)).

nvars(N) ->
    nvars_acc(N, []).

nvars_acc(N, Xs) when N < 1 ->
    Xs;
nvars_acc(N, Xs) ->
    Var = list_to_atom(lists:concat(["v", N])),
    nvars_acc(N - 1, [Var | Xs]).
