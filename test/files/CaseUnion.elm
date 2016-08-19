module CaseUnion exposing (Foo(Bar,Baz), isBar)

type Foo x = Bar x | Baz

isBar : Foo x -> Bool
isBar foo =
    case foo of
        Bar _ -> True
        Baz -> False
