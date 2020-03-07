module MexicanWave exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)


uppercase index string =
    String.toList string
        |> List.indexedMap
            (\innerIndex char ->
                if innerIndex == index then
                    Char.toUpper char

                else
                    char
            )
        |> String.fromList


wave input =
    String.join " " <| List.indexedMap (\index char -> uppercase index input) <| String.toList input


update msg model =
    case msg of
        InputField value ->
            { model | input = value }

        Click ->
            { model | output = wave model.input }


type Msg
    = InputField String
    | Click


type alias Model =
    { input : String, output : String }


init : Model
init =
    Model "" ""


view model =
    div []
        [ input [ onInput InputField, style "margin" "10px" ] []
        , button [ onClick Click, style "margin" "10px" ] [ text "Click" ]
        , text model.output
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
