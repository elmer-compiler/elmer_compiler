-module('Elm.TupleAccess').

-export([ access/0 ]).

access() ->
    elmer_runtime:partial(fun (V__p0) -> 
                                  V__p1 = V__p0,
                                  element(2, element(2, V__p1))
                          end, 1).
