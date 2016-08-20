-module('Elm.CaseUnionPosition').

-export([
         'Three'/0, 'isNine'/0
        ]).

'Three'() -> elmer_runtime:partial(fun (V_a, V_b, V_c) -> {'Three', {V_a, V_b, V_c}} end, 3).

isNine() ->
    elmer_runtime:partial(fun (V_n) ->
                                  V__p0 = V_n,
                                  case { element(1, element(2, V__p0)),
                                         element(2, element(2, V__p0)),
                                         element(3, element(2, V__p0)) } of
                                      {3, 3, 3} -> true;
                                      _ -> false
                                  end
                          end, 1).
