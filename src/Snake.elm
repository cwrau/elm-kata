module Snake exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events exposing (onKeyPress, onResize)
import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Task
import Time
import Tuple exposing (first, second)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            case char of
                'a' ->
                    DirectionChange Left

                'd' ->
                    DirectionChange Right

                _ ->
                    Ignore

        _ ->
            Ignore


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 200 Tick
        , onKeyPress keyDecoder
        , onResize SizeChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                ( xDir, yDir ) =
                    case model.dir of
                        North ->
                            ( 0, -1 )

                        South ->
                            ( 0, 1 )

                        West ->
                            ( -1, 0 )

                        East ->
                            ( 1, 0 )

                snakeTail : List Segment
                snakeTail =
                    model.snake
                        |> List.reverse
                        |> List.tail
                        |> Maybe.withDefault []
                        |> List.reverse

                snakeHead : Segment
                snakeHead =
                    model.snake
                        |> List.head
                        |> Maybe.withDefault (Segment 0 0)
                        |> (\head ->
                                Segment (head.x + xDir) (head.y + yDir)
                           )

                newModel =
                    let
                        ( initSnake, initFood ) =
                            initGame
                    in
                    if
                        (snakeTail |> List.any (\s -> s.x == snakeHead.x && s.y == snakeHead.y))
                            || (snakeHead.y >= (model.size.height // 10))
                            || (snakeHead.y < 0)
                            || (snakeHead.x >= (model.size.width // 10))
                            || (snakeHead.x < 0)
                    then
                        { model | snake = initSnake, food = initFood, dir = South, deathCount = model.deathCount + 1, eatenFoodCount = 0 }

                    else
                        let
                            food =
                                model.food |> List.partition (\f -> f == snakeHead)
                        in
                        if List.length (first food) > 0 then
                            let
                                addedTail =
                                    model.snake |> List.reverse |> List.head |> Maybe.withDefault (Segment 0 0)
                            in
                            { model | eatenFoodCount = model.eatenFoodCount + 1, snake = [ snakeHead ] ++ snakeTail ++ List.repeat 3 addedTail, food = second food }

                        else
                            { model | snake = [ snakeHead ] ++ snakeTail }
            in
            ( newModel, Cmd.none )

        DirectionChange directionChange ->
            let
                newModel =
                    case ( model.dir, directionChange ) of
                        ( North, Left ) ->
                            { model | dir = West }

                        ( North, Right ) ->
                            { model | dir = East }

                        ( South, Left ) ->
                            { model | dir = East }

                        ( South, Right ) ->
                            { model | dir = West }

                        ( West, Left ) ->
                            { model | dir = South }

                        ( West, Right ) ->
                            { model | dir = North }

                        ( East, Left ) ->
                            { model | dir = North }

                        ( East, Right ) ->
                            { model | dir = South }
            in
            ( newModel, Cmd.none )

        Ignore ->
            ( model, Cmd.none )

        SizeChanged width height ->
            ( { model | size = WindowSize width height }, Cmd.none )

        ViewportChanged vp ->
            ( { model | size = WindowSize (vp.viewport.width |> round) (vp.viewport.height |> round) }, Cmd.none )


type DirectionChange
    = Left
    | Right


type Msg
    = Tick Time.Posix
    | DirectionChange DirectionChange
    | Ignore
    | SizeChanged Int Int
    | ViewportChanged Viewport


type Direction
    = North
    | South
    | West
    | East


type alias Segment =
    { x : Int, y : Int }


type alias Food =
    { x : Int, y : Int }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { dir : Direction
    , snake : List Segment
    , food : List Food
    , size : WindowSize
    , deathCount : Int
    , eatenFoodCount : Int
    }


initGame : ( List Segment, List Food )
initGame =
    ( List.range 0 10 |> List.map (\i -> Segment 0 -i)
    , [ ( 20, 20 ), ( 78, 13 ), ( 90, 43 ), ( 23, 67 ), ( 45, 45 ) ] |> List.map (\( i, j ) -> Food i j)
    )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( snake, food ) =
            initGame
    in
    ( Model South
        snake
        food
        (WindowSize 100 100)
        0
        0
    , Task.perform ViewportChanged Dom.getViewport
    )


segmentToDiv : Segment -> Html msg
segmentToDiv segment =
    div
        [ style "position" "absolute"
        , style "top" (((segment.y * 10) |> String.fromInt) ++ "px")
        , style "left" (((segment.x * 10) |> String.fromInt) ++ "px")
        , style "height" "10px"
        , style "width" "10px"
        , style "background" "blue"
        ]
        []


foodToDiv : Food -> Html msg
foodToDiv food =
    div
        [ style "position" "absolute"
        , style "top" (((food.y * 10) |> String.fromInt) ++ "px")
        , style "left" (((food.x * 10) |> String.fromInt) ++ "px")
        , style "height" "10px"
        , style "width" "10px"
        , style "background" "red"
        , style "border-radius" "5px"
        ]
        []


view model =
    let
        snakeDivs =
            model.snake
                |> List.map segmentToDiv

        foodDivs =
            model.food
                |> List.map foodToDiv
    in
    div []
        [ br [] []
        , text ("snake: " ++ Debug.toString model.snake)
        , br [] []
        , text ("dir: " ++ Debug.toString model.dir)
        , br [] []
        , text ("width: " ++ Debug.toString model.size.width)
        , br [] []
        , text ("height: " ++ Debug.toString model.size.height)
        , br [] []
        , text ("deaths: " ++ Debug.toString model.deathCount)
        , br [] []
        , text ("food: " ++ Debug.toString model.eatenFoodCount)
        , div [] snakeDivs
        , div [] foodDivs
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
