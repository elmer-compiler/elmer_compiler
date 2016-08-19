-module('Elm.CaseInlineDataAccess').

-export([ uncurry/0 ]).

uncurry() ->
    elmer_runtime:partial(fun (V_f, V__p0) ->
                                  V__p1 = V__p0,
                                  V_f([ element(1, element(2, V__p1)),
                                        element(2, element(2, V__p1)) ])
                          end, 2).

