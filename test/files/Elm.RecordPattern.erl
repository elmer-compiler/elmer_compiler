-module('Elm.RecordPattern').

-export([ sum/0 ]).

sum() ->
    elmer_runtime:partial(fun (V__p0) ->
                                  V__p1 = V__p0,
                                  ('Elm.Basics':'+'())(
                                    [
                                     maps:get(x, V__p1),
                                     maps:get(y, V__p1)
                                    ])
                          end, 1).
