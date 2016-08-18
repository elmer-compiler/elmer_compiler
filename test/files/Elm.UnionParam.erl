-module('Elm.UnionParam').

-export([ 'Bar'/0 ]).

'Bar'() ->
    elmer_runtime:partial(fun (V_a) ->
                                  {'Bar', {V_a}}
                          end, 1).

