-module('Elm.CaseList').

-export([ isEmpty/0 ]).

isEmpty() ->
    elmer_runtime:partial(fun (_list) ->
                                  nil
                          end, 1).
