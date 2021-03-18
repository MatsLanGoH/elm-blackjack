module Main exposing (..)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, height, padding, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias Model =
    { playerCards : List Card
    , stackedCards : List Card
    }


type alias Card =
    { suit : Suit
    , rank : Rank
    }


type Suit
    = Spade
    | Heart
    | Diamond
    | Club


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


init : ( Model, Cmd Msg )
init =
    ( { playerCards =
            [ Card Spade Ace
            , Card Heart Two
            , Card Diamond Three
            , Card Club Four
            , Card Club King
            ]
      , stackedCards = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


mainView : Model -> Html Msg
mainView model =
    Element.layout []
        (gameView model)


gameView : Model -> Element Msg
gameView model =
    column
        [ Background.color (rgb255 0 185 0)
        , width <| fill
        , padding 10
        ]
        [ gameHeaderView
        , gameDealerView model
        , gamePlayerView model
        , gameActionsView model
        ]


gameHeaderView : Element msg
gameHeaderView =
    row
        [ width <| fill
        ]
        [ el [ centerX ] (text "Blackjack")
        ]


gameDealerView : Model -> Element msg
gameDealerView model =
    row
        [ width <| fill
        ]
        [ el
            [ alignLeft
            , Font.color <| rgb 1 1 0
            ]
            (text "Dealer Hand")
        , el [ alignRight ] <| text "Dealer Game Status"
        ]


gamePlayerView : Model -> Element msg
gamePlayerView model =
    row
        [ width <| fill
        ]
        [ el
            [ alignLeft
            , Font.color <| rgb 1 1 1
            , width <| fill
            ]
          <|
            column
                [ width <| fill ]
                [ text "Player Hand"
                , cardsView model.playerCards
                ]
        , el [ alignRight ] <| text "Player Game Status"
        ]


gameActionsView : Model -> Element Msg
gameActionsView model =
    -- TODO: Check actual Blackjack rules
    column
        [ centerX
        , spacing 10
        ]
        [ row [ centerX, centerY ] [ text "Game Actions" ]
        , row []
            [ actionButton "Draw"
            , actionButton "Hit"
            , actionButton "Stand"
            , actionButton "Deal"
            ]
        ]


actionButton : String -> Element Msg
actionButton label =
    Input.button
        [ Background.color <| rgb255 238 238 238
        , Element.focused [ Background.color <| rgb255 238 0 238 ]
        , height <| px 30
        , width <| px 80
        , Border.rounded 10
        ]
        { onPress = Nothing
        , label = text label
        }



---- CARD HELPERS ----


cardsView : List Card -> Element msg
cardsView cards =
    row
        [ Background.color <| rgb 1 1 1
        ]
    <|
        List.map cardItem cards


cardItem : Card -> Element msg
cardItem card =
    let
        suitColor =
            case card.suit of
                Heart ->
                    rgb255 205 0 0

                Diamond ->
                    rgb255 205 0 0

                _ ->
                    rgb255 20 20 20
    in
    el
        [ Font.size 200
        , Font.center
        , Font.color <| suitColor
        , Font.alignRight
        ]
        (text <| cardToString card)


cardToString : Card -> String
cardToString card =
    -- Turn cards into Unicode symbols
    -- https://en.wikipedia.org/wiki/Playing_cards_in_Unicode
    let
        baseCard =
            0x0001F0A1

        suitOffset =
            case card.suit of
                Heart ->
                    0x00

                Spade ->
                    0x10

                Diamond ->
                    0x20

                Club ->
                    0x30

        rankOffset =
            case card.rank of
                Ace ->
                    0

                Two ->
                    1

                Three ->
                    2

                Four ->
                    3

                Five ->
                    4

                Six ->
                    5

                Seven ->
                    6

                Eight ->
                    7

                Nine ->
                    8

                Ten ->
                    9

                Jack ->
                    10

                -- We don't want a Knight in our set
                -- Knight -> 11
                Queen ->
                    12

                King ->
                    13
    in
    baseCard + suitOffset + rankOffset |> Char.fromCode |> String.fromChar



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = mainView
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
