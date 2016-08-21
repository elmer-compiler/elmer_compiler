-module('Elm.RecordAccess').

-export([ access/0 ]).

access() ->
    maps:get(x, point()).

point() ->
    #{ x => 3, y => 4}.
