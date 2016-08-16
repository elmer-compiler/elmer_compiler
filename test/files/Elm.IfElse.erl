-module('Elm.IfElse').

-export([ power/0 ]).

power() ->
    elmer_runtime:partial(fun (V_level) ->
                                  case ('Elm.Basics':'>'())([V_level, 9000]) of
                                      true  -> <<"Over 9000!">>;
                                      _else -> <<"meh">>
                                  end
                          end, 1).
