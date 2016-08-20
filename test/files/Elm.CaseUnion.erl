-module('Elm.CaseUnion').

-export([ 'Bar'/0, 'Baz'/0, isBar/0 ]).

'Bar'() ->
    elmer_runtime:partial(fun (V_a) ->
                                  {'Bar', {V_a}}
                          end, 1).

'Baz'() ->
    {'Baz', {}}.

isBar() ->
    elmer_runtime:partial(fun (V_foo) ->
                                  V__p0 = V_foo,
                                  case {V__p0} of
                                      {{'Bar', _}} -> true;
                                       _ -> false
                                  end
                          end, 1).
