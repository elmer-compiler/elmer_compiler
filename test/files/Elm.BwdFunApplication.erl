-module('Elm.BwdFunApplication').

-export([ greet/0 ]).

greet() ->
    elmer_runtime:partial(fun(V_name) ->
                                  ('Elm.String':toUpper())([('Elm.Basics':'++'())([<<"Hello ">>,
                                                                                   V_name])])
                          end,
                          1).
