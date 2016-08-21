-module('Elm.RecordTypeAlias').

-export([ 'Location'/0 ]).

'Location'() ->
    elmer_runtime:partial(fun (V_a, V_b) ->
                                  #{ x => V_a, y => V_b }
                          end, 2).
