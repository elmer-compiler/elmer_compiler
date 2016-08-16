module IfElseNested exposing (power)

power : number -> String
power level =
    if level < 10 then
        "Mr Satan!"
    else if level > 9000 then
        "Over 9000!"
    else
        "meh"
