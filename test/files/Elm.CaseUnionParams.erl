-module('Elm.CaseUnionParams').

-export([
         'FirstWeekDay'/0,
         'LastWeekDay'/0,
         'Saturday'/0,
         'Sabado'/0,
         'Sunday'/0,
         'Domingo'/0,
         isSpanish/0
        ]).

'FirstWeekDay'() ->
    elmer_runtime:partial(fun (V_a) ->
                                  {'FirstWeekDay', {V_a}}
                          end, 1).
'LastWeekDay'() ->
    elmer_runtime:partial(fun (V_a) ->
                                  {'LastWeekDay', {V_a}}
                          end, 1).

'Saturday'() -> {'Saturday', {}}.

'Sabado'() -> {'Sabado', {}}.

'Sunday'() -> {'Sunday', {}}.

'Domingo'() -> {'Domingo', {}}.

isSpanish() ->
    elmer_runtime:partial(fun (V_d) ->
                                  V__p0 = V_d,
                                  case {V__p0} of
                                      {{'FirstWeekDay', _}} ->
                                          case {element(1, element(2, V__p0))} of
                                              {{'Domingo', _}} -> true;
                                              _ -> false
                                          end;
                                      _ ->
                                          case {element(1, element(2, V__p0))} of
                                              {{'Sabado', _}} -> true;
                                              _ -> false
                                          end
                                  end
                          end, 1).
