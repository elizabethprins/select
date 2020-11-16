module Main exposing (Model, Msg(..), init, main, update, view)

import Animations
import Animator
import Arrow
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onFocus, onInput, onSubmit)
import Json.Decode as Decode
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AnimationsMsg (Animations.subscriptions model.animations)
        , dropdownSubscription model.animations
        ]


dropdownSubscription : Animations.State -> Sub Msg
dropdownSubscription { dropdown } =
    if Animator.current dropdown then
        Sub.batch
            [ Browser.Events.onKeyUp outsideDropdown
            , Browser.Events.onClick outsideDropdown
            ]

    else
        Sub.none



-- MODEL


type alias Model =
    { animations : Animations.State
    , selected : Maybe String
    , options : List String
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { animations = Animations.init
      , selected = Nothing
      , options = optionList
      , title = "Dropdown component"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | AnimationsMsg Animations.Msg
    | FocusOn String
    | SetDropdown Bool
    | OnInput String
    | Select String
    | OnSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AnimationsMsg msg_ ->
            ( { model | animations = Animations.update msg_ model.animations }
            , Cmd.none
            )

        FocusOn id ->
            ( model
            , Task.attempt (always NoOp) <|
                Dom.focus id
            )

        SetDropdown bool ->
            ( { model | animations = Animations.dropdown bool model.animations }
            , Cmd.none
            )

        OnInput input ->
            let
                animations =
                    if Animator.current model.animations.dropdown then
                        model.animations

                    else
                        Animations.dropdown True model.animations

                selected =
                    if List.member input optionList then
                        Just input

                    else
                        Nothing
            in
            ( { model
                | animations = animations
                , selected = selected
                , options = List.filter (String.startsWith input) optionList
              }
            , Cmd.none
            )

        Select option ->
            ( { model
                | selected = Just option
                , options = optionList
              }
            , Task.attempt (always NoOp) <|
                Dom.focus "submit"
            )

        OnSubmit ->
            ( { model | title = Maybe.withDefault "Invalid selection" model.selected }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dropdown component"
    , body =
        [ main_ [ class "main" ]
            [ header []
                [ h1 [] [ text model.title ] ]
            , article []
                [ viewSelect model ]
            ]
        ]
    }


viewSelect : Model -> Html Msg
viewSelect model =
    let
        valueAttr =
            case model.selected of
                Just selected ->
                    [ value selected ]

                Nothing ->
                    []

        onKeyDown keyCode =
            case keyCode of
                40 ->
                    --arrow down
                    FocusOn "0"

                _ ->
                    NoOp
    in
    Html.form [ class "dropdown", onSubmit OnSubmit ]
        [ label [ for "input", class "visually-hidden" ]
            [ text "Select something" ]
        , div [ id "input-wrapper", class "input-wrapper" ]
            [ input
                ([ id "input"
                 , name "input"
                 , onInput OnInput
                 , onFocus (SetDropdown True)
                 , on "keydown" (Decode.map onKeyDown keyCode)
                 , autocomplete False
                 , placeholder "Select something"
                 ]
                    ++ valueAttr
                )
                []
            , Arrow.view
            , viewOptions model
            ]
        , button [ id "submit" ] [ text "submit" ]
        ]


viewOptions : Model -> Html Msg
viewOptions model =
    let
        tabindex_ =
            if Animator.current model.animations.dropdown then
                0

            else
                -1
    in
    Animations.viewDropdown model.animations [] <|
        List.indexedMap (viewOption tabindex_ (List.length model.options - 1))
            model.options


viewOption : Int -> Int -> Int -> String -> Html Msg
viewOption tabindex_ lastIndex index option_ =
    let
        onKeyDown keyCode =
            let
                next i =
                    if index + i == -1 || index + i == lastIndex + 1 then
                        "input"

                    else
                        String.fromInt (index + i)

                focus =
                    FocusOn << next
            in
            case keyCode of
                38 ->
                    --arrow up
                    focus -1

                40 ->
                    --arrow down
                    focus 1

                13 ->
                    --enter
                    Select option_

                32 ->
                    --space
                    Select option_

                _ ->
                    NoOp
    in
    li
        [ id (String.fromInt index)
        , tabindex tabindex_
        , onClick (Select option_)
        , on "keydown" (Decode.map onKeyDown keyCode)
        ]
        [ span [] [ text option_ ] ]



-- DECODE


outsideDropdown : Decode.Decoder Msg
outsideDropdown =
    Decode.field "target" decodeIsOutside
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed (SetDropdown False)

                else
                    Decode.fail "is inside target"
            )


decodeIsOutside : Decode.Decoder Bool
decodeIsOutside =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if id == "input-wrapper" then
                        Decode.succeed False

                    else
                        Decode.fail "check parentNode"
                )
        , Decode.lazy
            (\_ -> Decode.field "parentNode" decodeIsOutside)
        , Decode.succeed True
        ]



-- HELPERS


optionList : List String
optionList =
    [ "subject"
    , "objective"
    , "stand"
    , "incident"
    , "homework"
    , "spring"
    , "leg"
    , "community"
    , "message"
    , "you"
    , "combine"
    , "opening"
    , "return"
    , "contribution"
    , "hospital"
    , "design"
    , "heart"
    , "leave"
    , "week"
    , "studio"
    ]
