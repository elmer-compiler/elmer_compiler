module UnionAccess exposing (Foo(Bar), access)

type Foo = Bar Bool

access : Foo -> Bool
access (Bar bool) =
    bool
