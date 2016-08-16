-module('Elm.ConsLit').

-export([ lit/0 ]).

lit() ->
    ('Elm.List':'::'())([1, [2, 3, 4]]).
