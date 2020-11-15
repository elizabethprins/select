module Animations exposing
    ( Msg
    , State
    , dropdown
    , init
    , subscriptions
    , update
    , viewDropdown
    )

import Animator
import Animator.Css
import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)
import Time



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions state =
    animator |> Animator.toSubscription Tick state


animator : Animator.Animator State
animator =
    Animator.animator
        |> Animator.Css.watching .dropdown
            (\newAnimation state ->
                { state | dropdown = newAnimation }
            )



-- MODEL


type alias State =
    { dropdown : Animator.Timeline Bool }


init : State
init =
    { dropdown = Animator.init False }



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> State -> State
update msg state =
    case msg of
        Tick newTime ->
            Animator.update newTime animator state



-- ANIMATIONS


dropdown : Bool -> State -> State
dropdown bool state =
    { state
        | dropdown =
            state.dropdown
                |> Animator.go Animator.verySlowly bool
    }



-- VIEW


viewDropdown : State -> List (Attribute msg) -> List (Html msg) -> Html msg
viewDropdown state =
    Animator.Css.node "ul"
        state.dropdown
        [ opacity
        , transform "translateY({0}em)" -0.25
        ]



-- ATTRIBUTE STATES


opacity : Animator.Css.Attribute Bool
opacity =
    Animator.Css.opacity <|
        \currentState ->
            if currentState then
                Animator.at 1

            else
                Animator.at 0


transform : String -> Float -> Animator.Css.Attribute Bool
transform translation position =
    Animator.Css.style "transform"
        (\x -> interpolate translation [ String.fromFloat x ])
        (\currentState ->
            if currentState then
                Animator.at 0

            else
                Animator.at position
        )
