-module('Elm.RecordUpdate').

-export([ update/0 ]).

update() ->
    (point())#{ x := 6, y := 8}.

point() ->
    #{ x => 3, y => 4}.
