port module Main exposing (main)

import Browser
import Html exposing (Html, text, div, h1, img, button, input)
import Html.Attributes exposing (src, class, placeholder, type_)
import Array
import List
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import WebSocket

---- MODEL ----

type Player = CrossPlayer | CirclePlayer
type Square = Empty | Marked(Player)
type GameMode = SinglePlayer | Multiplayer
type GameState = 
    ShowMenu 
    | CreatingRoom
    | JoiningRoom
    | WaitingOnotherPlayer 
    | Playing 
    | Winner(Player) 
    | Draw

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
        playerName: String,
        oponentName: String,
        gameMode: GameMode,
        roomId: String,
        host: String,
        matrix: Array.Array(Array.Array(Square))
    }

type alias Flags =
    {
        host: String,
        joiningRoom: Bool,
        roomId: String,
        oponentName: String
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {
        state = if flags.joiningRoom then JoiningRoom else ShowMenu,
        gameMode = if flags.joiningRoom then Multiplayer else SinglePlayer,
        current = if flags.joiningRoom then CrossPlayer else CirclePlayer,
        pointsCross = 0,
        pointsCircle = 0,
        playerName = "",
        oponentName= flags.oponentName,
        roomId = flags.roomId,
        host = flags.host,
        matrix = emptyMatrix
    }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp 
    | Mark(Int, Int) 
    | Restart 
    | StartSinglePlayer 
    | WebsocketIn String
    | CreateRoom
    | JoinRoom
    | OnPlayerName String
    | CopyJoinUrl
    | StartMultiplayer

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
                }, Cmd.none )
        Restart ->
            ({
                model |
                matrix = emptyMatrix,
                current = if model.state == Winner(CrossPlayer) then CirclePlayer else CrossPlayer,
                state = Playing
            }, Cmd.none )
        StartSinglePlayer ->
            ({
                model | state = Playing, gameMode = SinglePlayer 
            }, Cmd.none)
        StartMultiplayer -> 
            (
                { model | gameMode = Multiplayer }, Cmd.none
            )
        OnPlayerName name -> 
             (
                { model | playerName = name }, Cmd.none
            )
        CreateRoom ->
            ( { model | state = CreatingRoom }, websocketOut ( WebSocket.createRoom model.playerName ) )
        JoinRoom ->
            ( { model | state = Playing }, websocketOut ( WebSocket.joinRoom model.roomId model.playerName ) )
        WebsocketIn(wsMsg) ->
            let 
                socketEvent = (WebSocket.decodeMesage wsMsg)
            in
                case socketEvent.eventType of 
                    "created" ->
                        ({ model | roomId = socketEvent.room, state = WaitingOnotherPlayer }, Cmd.none )
                    "joined" ->
                        ({ model | state = Playing, oponentName = socketEvent.playerName }, Cmd.none)
                    _ ->
                        ( model, Cmd.none )
        CopyJoinUrl -> 
            (model, copy (joinRoomUrl model))
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
    case model.state of 
        ShowMenu ->
            renderMenu model
        JoiningRoom -> 
            renderJoiningRoom model
        CreatingRoom ->
            renderRoomCreating model
        WaitingOnotherPlayer -> 
            renderWaitingScreen model
        _ ->
            div [] [
                renderHeader model
            ,   div [ class "matrix" ] ((Array.indexedMap renderRow model.matrix) |> Array.toList)
            ,   renderGameEndedOverlay model
            ]

renderJoiningRoom : Model -> Html Msg
renderJoiningRoom model =
    div [ class "menu-container" ] [
                div [ class "menu-title" ] [ text "Enter Name" ]
            ,   div [ class "actions-container" ] [
                    input [ type_ "text", placeholder "Enter your name", onInput OnPlayerName ] []
                ,   button [ class "btn menu-btn", onClick JoinRoom ] [ text "Join" ]
            ]
        ]

renderRoomCreating : Model -> Html Msg
renderRoomCreating model =
    div [ class "menu-container" ] [
        div [ class "menu-title" ] [ text "Creating room .. " ]
    ]

renderWaitingScreen : Model -> Html Msg 
renderWaitingScreen model = 
    div [ class "menu-container" ] [
        div [ class "menu-title" ] [ text "Room created" ]
    ,   div [ class "menu-p" ] [  text  "Waiting another player to join"  ]
    ,   div [ class "menu-p" ] [  text  "send the url to the player you want to play with." ]
    ,   div [ class "actions-container" ] [
            button [ class "btn menu-btn", onClick CopyJoinUrl ] [ text "Copy Join Url" ]
    ]
    ]

renderMenu : Model -> Html Msg
renderMenu model = 
    case model.gameMode of
        Multiplayer ->
             div [ class "menu-container" ] [
                div [ class "menu-title" ] [ text "Enter Name" ]
            ,   div [ class "actions-container" ] [
                    input [ type_ "text", placeholder "Enter your name", onInput OnPlayerName ] []
                ,   button [ class "btn menu-btn", onClick CreateRoom ] [ text "Start" ]
            ]
            ]
        _ ->
            div [ class "menu-container" ] [
                div [ class "menu-title" ] [ text "Menu" ]
            ,   div [ class "actions-container" ] [
                    button [ 
                        class "btn menu-btn" 
                    ,   onClick StartSinglePlayer 
                    ] [ text "Single Player" ]
                ,   button [ 
                        class "btn menu-btn" 
                    ,   onClick StartMultiplayer 
                    ] [ text "Multiplayer" ]
                ]
            ]

renderHeader: Model -> Html Msg
renderHeader model = 
    div [ class "header" ] [ 
                text (
                    (playerName CirclePlayer model)
                    ++ " "
                    ++ (String.fromInt model.pointsCircle)
                    ++ " - " 
                    ++ (String.fromInt model.pointsCross) 
                    ++ " "
                    ++ (playerName CrossPlayer model)
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
                Winner player ->
                    div [ class "winner-title" ] [ 
                        text ((playerName player model) ++ " Wins")
                    ,   renderRestartButton model
                    ]
                _ ->
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

playerName : Player -> Model -> String
playerName player model =
    case player of 
        CirclePlayer ->
            if model.gameMode == SinglePlayer then "Circle" else model.playerName
        CrossPlayer -> 
            if model.gameMode == SinglePlayer then "Cross" else model.oponentName

joinRoomUrl : Model -> String
joinRoomUrl model = 
    model.host ++ "?room=" ++ model.roomId ++ "&playerName=" ++ model.playerName 

---- PORTS ----

port websocketIn : (String -> msg) -> Sub msg
port websocketOut : String -> Cmd msg
port copy :  String -> Cmd msg
        

---- PROGRAM ----

subscriptions : Model -> Sub Msg
subscriptions model =
    websocketIn WebsocketIn


main :Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = \f -> (init f)
        , update = update
        , subscriptions = subscriptions
        }
