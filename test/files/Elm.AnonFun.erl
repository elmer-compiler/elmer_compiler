-module('Elm.AnonFun').

-export([ anon/0 ]).

anon() ->
    elmer_runtime:partial(fun (V_x) ->
                                  ('Elm.Basics':'*'())([V_x, 2])
                          end, 1).
