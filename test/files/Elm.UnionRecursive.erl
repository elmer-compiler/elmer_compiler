-module('Elm.UnionRecursive').

-export([ 'Empty'/0, 'Node'/0 ]).

'Empty'() ->
    {'Empty', {}}.

'Node'() ->
    elmer_runtime:partial(fun (V_a, V_b) ->
                                  {'Node', {V_a, V_b}}
                          end, 2).
