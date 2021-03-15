module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Html exposing (Html)



-- TODO: Clear active tiles
-- TODO: Randomly generate and prepend new dots
---- MODEL ----


type alias Cell =
    { row : Int
    , col : Int
    , dot : Dot
    }


type alias GridColumn =
    List Dot


type alias Active =
    List Cell


type alias Grid =
    List GridColumn


type Model
    = Loading
    | Running GameState
    | Error


type Dot
    = GreenDot
    | BlueDot
    | RedDot


type alias GameState =
    { grid : Grid
    , active : Active
    }


init : ( Model, Cmd Msg )
init =
    ( Running initGameState
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Choose Dot
    | NewGrid
    | SelectCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Choose tile ->
            ( model, Cmd.none )

        SelectCell cell ->
            case model of
                Running gameState ->
                    ( Running <| activateCell cell gameState
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NewGrid ->
            ( model, Cmd.none )


activateCell : Cell -> GameState -> GameState
activateCell cell gameState =
    let
        cellAdjacent =
            gameState.active
                |> List.head
                |> Maybe.withDefault { row = 0, col = 0, dot = RedDot }
                |> isCellAdjacent cell
    in
    if List.isEmpty gameState.active then
        { gameState | active = [ cell ] }

    else if List.any (\c -> cell.dot == c.dot) gameState.active && cellAdjacent then
        { gameState | active = cell :: gameState.active }

    else
        gameState


isCellAdjacent : Cell -> Cell -> Bool
isCellAdjacent selected last =
    List.any (\fn -> fn selected last)
        [ isCellBelow
        , isCellAbove
        , isCellLeft
        , isCellRight
        ]


isCellBelow : Cell -> Cell -> Bool
isCellBelow selected last =
    (last.row == selected.row - 1)
        && (last.col == selected.col)


isCellAbove : Cell -> Cell -> Bool
isCellAbove selected last =
    (last.row == selected.row + 1) && last.col == selected.col


isCellLeft : Cell -> Cell -> Bool
isCellLeft selected last =
    (last.col == selected.col + 1) && last.row == selected.row


isCellRight : Cell -> Cell -> Bool
isCellRight selected last =
    (last.col == selected.col - 1)
        && (last.row == selected.row)



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        case model of
            Loading ->
                el [] (text "Loading")

            Running gameState ->
                viewGameState gameState

            Error ->
                el [] (text "Error")


initGameState : GameState
initGameState =
    { grid =
        [ [ RedDot, RedDot, RedDot ]
        , [ RedDot, RedDot, RedDot ]
        , [ RedDot, RedDot, RedDot ]
        ]
    , active = []
    }


viewGameState : GameState -> Element Msg
viewGameState gameState =
    el [ width fill, centerX, centerY ] <|
        row [ width fill, spacing 30 ] <|
            List.indexedMap (genGridCol gameState.active) gameState.grid


genGridCol : Active -> Int -> GridColumn -> Element Msg
genGridCol active colNum gridRow =
    column [ width fill, height fill, spacing 30 ] <|
        List.indexedMap (genGridTile active colNum) gridRow


genGridTile : Active -> Int -> Int -> Dot -> Element Msg
genGridTile active colNum rowNum dot =
    let
        cell =
            { row = rowNum
            , col = colNum
            , dot = dot
            }

        isCellActive =
            List.any (\c -> c == cell)
    in
    el
        [ width fill
        , height (px 100)
        , tileColor (isCellActive active) dot
        , onClick (SelectCell cell)
        ]
        (text ("Row: " ++ String.fromInt rowNum ++ " Col: " ++ String.fromInt colNum))


tileColor : Bool -> Dot -> Attr decorative msg
tileColor active tile =
    let
        alpha =
            if active then
                0.25

            else
                1
    in
    case tile of
        RedDot ->
            Background.color (rgba255 255 0 0 alpha)

        GreenDot ->
            Background.color (rgba255 0 255 0 alpha)

        BlueDot ->
            Background.color (rgba255 0 0 255 alpha)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
