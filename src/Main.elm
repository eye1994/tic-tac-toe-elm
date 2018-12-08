module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)
import Array
import List
import Html.Events exposing (onClick)


---- MODEL ----

type Player = CrossPlayer | CirclePlayer
type Square = Empty | Marked(Player)

type alias Model =
    {
        current: Player,
        matrix: Array.Array(Array.Array(Square))
    }


init : ( Model, Cmd Msg )
init =
    ( {
        current = CirclePlayer,
        matrix = Array.fromList([
            [Empty, Empty, Empty] |> Array.fromList,
            [Empty, Empty, Empty] |> Array.fromList,
            [Empty, Empty, Empty] |> Array.fromList
        ])
    }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp | Mark(Int, Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Mark(x, y) -> 
            ({
                model | 
                matrix = (markCell x y model.matrix model.current),
                current = if model.current == CirclePlayer then CrossPlayer else CirclePlayer    
            }, Cmd.none)
        _ -> 
            ( model, Cmd.none )


markCell: Int -> Int -> Array.Array(Array.Array(Square)) -> Player -> Array.Array(Array.Array(Square))
markCell x y matrix player =
    let 
        row = Array.get x matrix
        rowUpdated = Array.set y (Marked(player)) (case row of
            Just value ->
                value
            Nothing ->
                Array.fromList([]))
    in
        Array.set x rowUpdated matrix


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "application-container" ]
        [   h1 [ class "title" ] [ text "Tic Tac Toe" ]
        ,   div [ class "matrix-container" ] [ renderMatrix model.matrix ]
        ]

renderMatrix : Array.Array(Array.Array(Square)) -> Html Msg
renderMatrix matrix =
    div [ class "matrix" ] ((Array.indexedMap renderRow matrix) |> Array.toList)

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
