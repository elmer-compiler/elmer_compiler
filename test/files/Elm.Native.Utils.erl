-module('Elm.Native.Utils').

-export([ append/0 ]).

append() ->
    elmer_runtime:partial(fun (V_a, V_b) ->
                                  erlang:iolist_to_binary([V_a, V_b])
                          end, 2).
