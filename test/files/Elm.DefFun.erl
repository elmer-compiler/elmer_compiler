-module('Elm.DefFun').

-export([ 'fun'/0 ]).

'fun'() ->
    (elmer_runtime:partial(fun (V_a, V_b) ->
                                   ('Elm.Basics':clamp())([V_a, V_b, 10])
                           end, 2)).
