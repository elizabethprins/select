module Arrow exposing (view)

import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Svg msg
view =
    svg
        [ width "30", height "18", viewBox "0 0 30 18" ]
        [ Svg.title [] [ text "dropdown arrow" ]
        , Svg.path
            [ d "M29.443 1.514c-.37-.374-.81-.561-1.318-.561H1.875c-.508 0-.947.187-1.318.56A1.823 1.823 0 000 2.844c0 .51.185.953.557 1.327L13.68 17.392c.372.374.811.561 1.319.561.508 0 .947-.187 1.318-.561L29.443 4.17c.37-.374.557-.816.557-1.328 0-.511-.186-.954-.557-1.328z"
            , fill "#150C3C"
            ]
            []
        ]
