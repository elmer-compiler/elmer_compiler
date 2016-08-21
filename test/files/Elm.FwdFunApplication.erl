-module('Elm.FwdFunApplication').

-export([ greet/0 ]).

greet() ->
    ('Elm.String':toUpper())([<<"hello">>]).
