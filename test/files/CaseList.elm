module CaseList exposing (first)

first : List a -> Maybe a
first list =
    case list of
        a :: b :: c -> Just a
        a :: [] -> Just a
        [] -> Nothing
