-module('Elm.ImportExposing').

-export([ header/2 ]).

header(X, Y) ->
    'Elm.Html':h1(X, Y).
