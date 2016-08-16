-module('Elm.DefFun').

-export([ 'fun'/0 ]).

'fun'() ->
    (elmer_runtime:partial(fun (V_x, V_y) ->
                                   ('Elm.Basics':'+'())([V_x, V_y])
                           end, 2))([2]).
