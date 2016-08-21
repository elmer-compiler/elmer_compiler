module RecordUpdate exposing (update)

point = { x = 3, y = 4 }

update =
    { point | x = 6, y = 8 }
