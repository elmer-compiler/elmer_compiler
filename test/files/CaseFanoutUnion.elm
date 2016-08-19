module CaseFanoutUnion exposing (Foo(Bar,Baz,Bat), isBar)

type Foo x = Bar x | Baz | Bat

isBar : Foo x -> Bool
isBar foo =
    case foo of
        Bar _ -> True
        Baz -> False
        Bat -> False
