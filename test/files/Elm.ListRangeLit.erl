-module('Elm.ListRangeLit').

-export([ lit/0 ]).

lit() ->
    elmer_runtime:list({range, 1, 4}).
