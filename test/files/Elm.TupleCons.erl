-module('Elm.TupleCons').

-export([ foo/0 ]).

foo() ->
    (elmer_runtime:partial(fun (V_v0, V_v1, V_v2, V_v3, V_v4, V_v5, V_v6, V_v7, V_v8) ->
                                  {'_Tuple9', {V_v0, V_v1, V_v2, V_v3, V_v4, V_v5, V_v6, V_v7, V_v8}}
                          end, 9))([1, 2, 3, 4, 5, 6, 7, 8, 9]).
