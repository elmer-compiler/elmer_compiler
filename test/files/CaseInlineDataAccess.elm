module CaseInlineDataAccess exposing (uncurry)

{-| Weird case from Elm Basics

This is weird case where the Elm compiler produces a case expression
but it has NO condition, just an inline leaf that will always be executed.

Elmer compiles it to just the leaf expression and no case as expected.
-}
uncurry : (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) =
  f a b
