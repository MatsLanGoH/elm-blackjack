module Main exposing (..)

import Browser
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, px, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as LE
import Random
import Random.List as RL



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
    ( { playerCards = []
      , stackedCards = []
      }
    , createShuffledStack
    )



---- UPDATE ----


type Msg
    = NoOp
    | PlayerDrawsCard
    | Deal
    | NewStack (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerDrawsCard ->
            let
                drawnCard =
                    List.take 1 model.stackedCards

                newStack =
                    List.drop 1 model.stackedCards

                newHand =
                    model.playerCards ++ drawnCard
            in
            ( { model
                | stackedCards = newStack
                , playerCards = newHand
              }
            , Cmd.none
            )

        NewStack newStack ->
            ( { model
                | stackedCards = newStack
                , playerCards = []
              }
            , Cmd.none
            )

        Deal ->
            ( model, createShuffledStack )

        NoOp ->
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
            , Font.color <| rgb 1 1 1
            , width <| fill
            ]
          <|
            column
                [ width <| fill ]
                [ text "Remaining cards"
                , text <| String.fromInt (List.length model.stackedCards)
                ]
        , el [ alignRight ] <| text "Dealer Game Status"
        ]


gamePlayerView : Model -> Element msg
gamePlayerView model =
    row
        [ width <| fill
        , height <| px 150
        ]
        [ column
            [ alignLeft
            , Font.color <| rgb 1 1 1
            , width <| fill
            , height <| fill
            ]
            [ el [ alignTop ] (text "Player Hand")
            , cardsView model.playerCards
            ]
        , column
            [ alignRight ]
            [ row [] [ text "Player Game Status" ]
            , row []
                [ text "Current Score: "
                , text <| String.fromInt <| calculateScore model.playerCards
                ]
            ]
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
            [ actionButton "Draw" PlayerDrawsCard
            , actionButton "Hit" NoOp
            , actionButton "Stand" NoOp
            , actionButton "Deal" Deal
            ]
        ]


actionButton : String -> Msg -> Element Msg
actionButton label action =
    Input.button
        [ Background.color <| rgb255 238 238 238
        , Element.focused [ Background.color <| rgb255 238 0 238 ]
        , height <| px 30
        , width <| px 80
        , Border.rounded 10
        ]
        { onPress = Just <| action
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
        [ Font.size 120
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
                Spade ->
                    0x00

                Heart ->
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


createStack : List Card
createStack =
    -- cf
    -- https://discourse.elm-lang.org/t/all-possible-combinations-of-a-product-type/4789/5
    LE.lift2 Card
        [ Heart, Spade, Diamond, Club ]
        [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


cardShuffler : List a -> Random.Generator (List a)
cardShuffler cards =
    RL.shuffle cards


createShuffledStack : Cmd Msg
createShuffledStack =
    Random.generate NewStack (cardShuffler createStack)



---- CARD SCORE LOGIC ----


rankValue : Rank -> Int
rankValue rank =
    case rank of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            10

        Queen ->
            10

        King ->
            10


sumByRanks : List Rank -> Int
sumByRanks ranks =
    ranks |> List.map rankValue |> List.sum


calculateScore : List Card -> Int
calculateScore cards =
    {--
    Score rules:
        * If we have an Ace and either 10/J/Q/K, ace counts as 11
        * Otherwise Ace counts as 1 
    --}
    let
        ranks =
            List.map (\card -> card.rank) cards

        pictureRanks =
            [ Ten, Jack, Queen, King ]
    in
    case LE.remove Ace ranks of
        [ card ] ->
            if List.member card pictureRanks then
                21

            else
                sumByRanks ranks

        _ ->
            sumByRanks ranks



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = mainView
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
