module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, button)
import Html.Attributes exposing (src, class)
import Array
import List
import Html.Events exposing (onClick)


---- MODEL ----

type Player = CrossPlayer | CirclePlayer
type Square = Empty | Marked(Player)
type GameState = Playing | Winner(Player) | Draw

emptyMatrix = Array.fromList([
                [Empty, Empty, Empty] |> Array.fromList,
                [Empty, Empty, Empty] |> Array.fromList,
                [Empty, Empty, Empty] |> Array.fromList
            ])

type alias Model =
    {
        current: Player,
        state: GameState,
        pointsCross: Int,
        pointsCircle: Int,
        matrix: Array.Array(Array.Array(Square))
    }


init : ( Model, Cmd Msg )
init =
    ( {
        state = Playing,
        current = CirclePlayer,
        pointsCross = 0,
        pointsCircle = 0,
        matrix = emptyMatrix
    }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp | Mark(Int, Int) | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Mark(x, y) ->
            let 
                nextMatrix = (markCell x y model.matrix model.current)
                nextGameState = computeNextState nextMatrix
            in
                ({
                    model | 
                    matrix = nextMatrix,
                    pointsCross = if nextGameState == Winner(CrossPlayer) then model.pointsCross + 1 else model.pointsCross,
                    pointsCircle = if nextGameState == Winner(CirclePlayer) then model.pointsCircle + 1 else model.pointsCircle,
                    state = nextGameState,
                    current = if model.current == CirclePlayer then CrossPlayer else CirclePlayer    
                }, Cmd.none)
        Restart ->
            ({
                model |
                matrix = emptyMatrix,
                current = if model.state == Winner(CrossPlayer) then CirclePlayer else CrossPlayer,
                state = Playing
            }, Cmd.none )
        NoOp -> 
            ( model, Cmd.none )


markCell: Int -> Int -> Array.Array(Array.Array(Square)) -> Player -> Array.Array(Array.Array(Square))
markCell x y matrix player =
    let 
        row = Array.get x matrix
        rowUpdated = Array.set y (Marked(player)) (maybeArrayToJust row)
    in
        Array.set x rowUpdated matrix

computeNextState: Array.Array(Array.Array(Square)) -> GameState
computeNextState matrix  =
    if (isWinning matrix CirclePlayer) then
        Winner(CirclePlayer)
    else if (isWinning matrix CrossPlayer) then 
        Winner(CrossPlayer)
    else if (isDraw matrix) then
        Draw
    else 
        Playing

isDraw: Array.Array(Array.Array(Square)) -> Bool
isDraw matrix = 
    let 
        excludeEmpty = Array.filter(\c -> (c /= Empty))
        rows = matrix |> Array.map(\r -> (excludeEmpty r))
        emptyRows = rows |> Array.filter(\r -> ((Array.length r) == 3))
    in
        (Array.length emptyRows) == 3

isWinning: Array.Array(Array.Array(Square)) -> Player -> Bool
isWinning matrix player = 
    let 
        row1 = maybeArrayToJust (Array.get 0 matrix)
        row2 = maybeArrayToJust (Array.get 1 matrix)
        row3 = maybeArrayToJust (Array.get 2 matrix)

        col1 = getColl 0 matrix
        col2 = getColl 1 matrix
        col3 = getColl 2 matrix

        c11 = matrix |> Array.get 0 |> maybeArrayToJust |> Array.get 0 |> maybeToJust
        c22 = matrix |> Array.get 1 |> maybeArrayToJust |> Array.get 1 |> maybeToJust
        c33 = matrix |> Array.get 2 |> maybeArrayToJust |> Array.get 2 |> maybeToJust

        c13 = matrix |> Array.get 0 |> maybeArrayToJust |> Array.get 2 |> maybeToJust
        c31 = matrix |> Array.get 2 |> maybeArrayToJust |> Array.get 0 |> maybeToJust

        d1 = c11 :: c22 :: c33 :: [] |> Array.fromList
        d2 = c13 :: c22 :: c31 :: [] |> Array.fromList
    in
        if (checkRow row1 player) || (checkRow row2 player) || (checkRow row3 player)
        || (checkRow col1 player) || (checkRow col2 player) || (checkRow col3 player) 
        || (checkRow d1 player) || (checkRow d2 player) then
            True
        else
            False

getColl:  Int -> Array.Array(Array.Array(Square)) -> Array.Array(Square)
getColl col matrix =
    matrix
    |> Array.map(\row -> (maybeToJust (Array.get col row) ))

checkRow: Array.Array(Square) -> Player -> Bool 
checkRow row player = 
    let 
        filter =  Array.filter (\cell -> cell == Marked(player)) row
    in
        (Array.length filter) == 3

maybeToJust: Maybe(Square) ->  Square
maybeToJust square =
    case square of 
        Just value ->
            value
        Nothing ->
            Empty

maybeArrayToJust: Maybe(Array.Array(Square)) ->  Array.Array(Square)
maybeArrayToJust row = 
    case row of
        Just value ->
            value
        Nothing ->
            Array.fromList([])

---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "application-container" ]
        [   h1 [ class "title" ] [ text "Tic Tac Toe" ]
        ,   div [ class "game-container" ] [ renderGame model ]
        ]

renderGame : Model -> Html Msg
renderGame model =
    div [] [
        renderHeader model
    ,   div [ class "matrix" ] ((Array.indexedMap renderRow model.matrix) |> Array.toList)
    ,   renderGameEndedOverlay model
    ]

renderHeader: Model -> Html Msg
renderHeader model = 
    div [ class "header" ] [ 
                text (
                    "Circle " 
                    ++ String.fromInt(model.pointsCircle)
                    ++ " - " 
                    ++ String.fromInt(model.pointsCross) 
                    ++ " Cross"
                ) 
            ]

renderGameEndedOverlay: Model -> Html Msg 
renderGameEndedOverlay model = 
    if model.state /= Playing then
        (div [ class "winner-overlay" ] [ 
            case model.state of 
                Draw -> 
                    div [ class "winner-title" ] [ 
                        text "Draw"
                    ,   renderRestartButton model
                    ]
                Winner(CirclePlayer) ->
                    div [ class "winner-title" ] [ 
                        text "Circle Wins" 
                    ,   renderRestartButton model
                    ]
                Winner(CrossPlayer) -> 
                    div [ class "winner-title" ] [ 
                        text "Cross wins"
                    ,   renderRestartButton model
                    ]
                Playing ->
                    text ""
        ])
    else
        div [] []

renderRestartButton: Model -> Html Msg
renderRestartButton model = 
    div [ class "btn container" ] [
        button [ 
            class "btn restart" 
        ,   onClick Restart
        ] [ 
            text "Restart" 
        ]
    ]

renderRow : Int -> Array.Array(Square) -> Html Msg
renderRow x row =
    let 
        render = renderCell(x)
        rows = Array.indexedMap render row
    in
        div [ class "row" ] (rows |> Array.toList)

renderCell : Int -> Int -> Square -> Html Msg
renderCell x y cell =
    case cell of
        Empty ->
            div [   class "cell empty"
                ,   onClick (Mark(x, y))
                ] []
        Marked player ->
            case player of 
                CrossPlayer ->
                    div [ class "cell marked marked-cross" ] [ text "X" ]
                CirclePlayer ->
                    div [ class "cell marked marked-circle" ] [ text "O"]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
