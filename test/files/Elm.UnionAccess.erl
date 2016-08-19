-module('Elm.UnionAccess').

-export([ 'Bar'/0, access/0 ]).

'Bar'() ->
    elmer_runtime:partial(fun (V_a) ->
                                  {'Bar', {V_a}}
                          end,1).

access() ->
    elmer_runtime:partial(fun (V__p0) ->
                                  V__p1 = V__p0,
                                  element(1, element(2, V__p1))
                          end, 1).
