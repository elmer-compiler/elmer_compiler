module ImportExposing exposing (header)

import Html exposing (h1)

header : List (Html.Attribute a) -> List (Html.Html a) -> Html.Html a
header = h1
