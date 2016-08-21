module BwdFunApplication exposing (greet)

import String

greet name =
    String.toUpper <| "Hello " ++ name
