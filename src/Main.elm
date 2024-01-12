module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (list, style)
import List
import List.Extra exposing (transpose, uncons, unconsLast)
import Time



-- MODEL


type GOL_Cell
    = Dead
    | Alive


type alias Model =
    List (List GOL_Cell)


type Msg
    = Tick



-- INIT


init : ( Model, Cmd Msg )
init =
    ( [ [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
      , [ Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead ]
      , [ Dead, Alive, Dead, Alive, Dead, Dead, Dead, Dead ]
      , [ Dead, Dead, Alive, Alive, Dead, Dead, Dead, Dead ]
      , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
      , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
      ]
    , Cmd.none
    )



-- LIST FUNCS


rotateL : List a -> List a
rotateL list =
    case uncons list of
        Just ( x, xs ) ->
            xs ++ [ x ]

        Nothing ->
            []


rotateR : List a -> List a
rotateR list =
    case unconsLast list of
        Just ( x, xs ) ->
            x :: xs

        Nothing ->
            []


rotateO : List (List a) -> List (List a)
rotateO list =
    list
        |> transpose
        |> rotateR
        |> transpose


rotateB : List (List a) -> List (List a)
rotateB list =
    list
        |> transpose
        |> rotateL
        |> transpose



-- GOL helper


toNum : GOL_Cell -> Int
toNum cell =
    case cell of
        Alive ->
            1

        Dead ->
            0


sumLists : List Int -> List Int -> List Int
sumLists fst snd =
    let
        aux a b acc =
            case ( a, b ) of
                ( ax :: axs, bx :: bxs ) ->
                    aux axs bxs ((ax + bx) :: acc)

                _ ->
                    List.reverse acc
    in
    aux fst snd []


sumLList : List (List Int) -> List (List Int) -> List (List Int)
sumLList fst snd =
    let
        aux a b acc =
            case ( a, b ) of
                ( ax :: axs, bx :: bxs ) ->
                    aux axs bxs (sumLists ax bx :: acc)

                _ ->
                    List.reverse acc
    in
    aux fst snd []



-- UPDATE


updateModel : Model -> Model
updateModel model =
    let
        rotations =
            [ rotateL, identity, rotateR ]

        vrotations =
            [ rotateO, identity, rotateB ]

        plusReduce ls =
            List.foldl sumLList (Maybe.withDefault [] (List.head ls)) (Maybe.withDefault [] (List.tail ls))

        nmodel =
            List.map (List.map toNum) model
    in
    List.map (\f -> f nmodel) rotations
        |> List.map (\ll -> List.map (\f -> f ll) vrotations)
        |> List.map plusReduce
        |> plusReduce
        |> List.map2
            (List.map2
                (\gol ->
                    \n ->
                        case ( gol, n ) of
                            -- alive + 2 neighbours
                            ( Alive, 3 ) ->
                                Alive

                            -- alive + 3 neighbours
                            ( Alive, 4 ) ->
                                Alive

                            -- Dead + 3 neighbours
                            ( Dead, 3 ) ->
                                Alive

                            _ ->
                                Dead
                )
            )
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( updateModel model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        size =
            50

        measure =
            "rem"
    in
    div
        [ style "display" "flex"
        , style "height" (String.fromInt size ++ measure)
        ]
        (List.map
            (\l ->
                div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "width" (String.fromInt (size // List.length l) ++ measure)
                    ]
                    (List.map
                        (\i ->
                            div
                                [ case i of
                                    Dead ->
                                        style "background-color" "black"

                                    _ ->
                                        style "background-color" "white"
                                , style "flex-grow" "1"
                                ]
                                []
                        )
                        l
                    )
            )
            model
        )


main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, view = view, update = update, subscriptions = \_ -> Time.every 50 (\_ -> Tick) }
