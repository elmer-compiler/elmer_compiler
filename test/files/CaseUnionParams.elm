module CaseUnionParams exposing (FirstDay(..), LastDay(..), WeekDay(..), isSpanish)

type FirstDay = Sunday  | Domingo
type LastDay = Saturday | Sabado

type WeekDay = FirstWeekDay FirstDay | LastWeekDay LastDay

isSpanish : WeekDay -> Bool
isSpanish d =
    case d of
        FirstWeekDay Domingo -> True
        LastWeekDay Sabado  -> True
        _ -> False
