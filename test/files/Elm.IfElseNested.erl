-module('Elm.IfElseNested').

-export([ power/0 ]).

power() ->
    elmer_runtime:partial(fun (V_level) ->
                                  case ('Elm.Basics':'<'())([V_level, 10]) of
                                      true -> <<"Mr Satan!">>;
                                      _else ->
                                          case ('Elm.Basics':'>'())([V_level, 9000]) of
                                              true  -> <<"Over 9000!">>;
                                              _else -> <<"meh">>
                                          end
                                  end
                          end, 1).
