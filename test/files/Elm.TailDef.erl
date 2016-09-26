-module('Elm.TailDef').

-export([rec/0]).

rec() ->
  (elmer_runtime:partial(fun (V_x) ->
                                 (rec())([V_x])
                         end, 1)).
