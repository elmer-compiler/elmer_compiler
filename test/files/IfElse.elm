module IfElse exposing (power)

power : number -> String
power level =
    if level > 9000 then
        "Over 9000!"
    else
        "meh"
