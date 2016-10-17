module LetInTailDef exposing (foo)

foo =
    let
        sum x y =
            if y == 0 then
                x
            else
                sum (x + y)  0
        val = sum 2 3
    in
        val
