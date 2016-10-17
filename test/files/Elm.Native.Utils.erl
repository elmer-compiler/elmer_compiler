-module('Elm.Native.Utils').

-export([ append/0 ]).

append() ->
    elmer_runtime:partial(fun (Xs, Ys) when is_binary(Xs) -> erlang:iolist_to_binary([Xs, Ys]);
                              (Xs, Ys) when is_list(Xs) -> lists:append(Xs, Ys)
                          end, 2).
