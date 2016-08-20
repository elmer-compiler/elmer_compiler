module CaseUnionPosition exposing (Nine(..), isNine)

type Nine = Three Int Int Int

isNine : Nine -> Bool
isNine n =
    case n of
        Three 3 3 3 -> True
        _ -> False
