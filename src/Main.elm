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
    { playerHand : List Card
    , dealerHand : List Card
    , stackedCards : List Card
    , gameStatus : GameStatus
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


type PlayerType
    = Player
    | Dealer


type GameStatus
    = Running
    | Stopped


init : ( Model, Cmd Msg )
init =
    ( { playerHand = []
      , dealerHand = []
      , stackedCards = []
      , gameStatus = Running
      }
    , createShuffledStack
    )



---- UPDATE ----


type Msg
    = NoOp
    | PlayerDrawsCard
    | Deal
    | RestartGame
    | NewStack (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerDrawsCard ->
            let
                drawnCard =
                    List.take 1 model.stackedCards

                updatedStack =
                    List.drop 1 model.stackedCards

                newHand =
                    model.playerHand ++ drawnCard

                gameStatus =
                    getGameStatus newHand
            in
            ( { model
                | stackedCards = updatedStack
                , playerHand = newHand
                , dealerHand = []
                , gameStatus = gameStatus
              }
            , Cmd.none
            )

        NewStack newStack ->
            ( { model
                | stackedCards = newStack
                , playerHand = []
                , dealerHand = []
                , gameStatus = Running
              }
            , Cmd.none
            )

        Deal ->
            ( { model
                | playerHand = []
                , dealerHand = []
                , gameStatus = Running
              }
            , Cmd.none
            )

        RestartGame ->
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
        , spacing 10
        ]
        [ gameHeaderView
        , gameInfoView model
        , gameProgressView model Dealer
        , gameProgressView model Player
        , gameActionsView model
        ]


gameHeaderView : Element msg
gameHeaderView =
    row
        [ width <| fill
        ]
        [ el [ centerX ] (text "Blackjack")
        ]


gameInfoView : Model -> Element msg
gameInfoView model =
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


gameProgressView : Model -> PlayerType -> Element msg
gameProgressView model playertype =
    viewStatus model playertype


viewStatus : Model -> PlayerType -> Element msg
viewStatus model playerType =
    let
        label =
            if playerType == Player then
                "Player"

            else
                "Dealer"

        hand =
            if playerType == Player then
                model.playerHand

            else
                model.dealerHand
    in
    row
        [ width <| fill
        , height <| px 150
        , padding 10
        , Background.color <| rgb255 30 222 30
        ]
        [ viewHand hand label
        , viewScore hand
        ]


viewHand : List Card -> String -> Element msg
viewHand hand label =
    column
        [ alignLeft
        , Font.color <| rgb 1 1 1
        , width <| fill
        , height <| fill
        ]
        [ el [ alignTop ] (text <| label ++ " Hand")
        , cardsView hand
        ]


viewScore : List Card -> Element msg
viewScore hand =
    column
        [ alignRight ]
        [ row []
            [ text "Current Score: "
            , text <| String.fromInt <| calculateScore hand
            ]
        , row []
            [ text <| calculateResultString hand
            ]
        ]


gameActionsView : Model -> Element Msg
gameActionsView model =
    -- TODO: Check actual Blackjack rules
    let
        drawButton =
            if model.gameStatus == Running then
                activeButton "Draw" PlayerDrawsCard

            else
                disabledButton "--" NoOp
    in
    column
        [ centerX
        , spacing 10
        ]
        [ row [ centerX, centerY ] [ text "Game Actions" ]
        , row []
            [ drawButton

            -- , activeButton "Hit" NoOp
            -- , activeButton "Stand" NoOp
            , activeButton "Deal" Deal
            , activeButton "Restart" RestartGame
            ]
        ]


activeButton : String -> Msg -> Element Msg
activeButton label action =
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


disabledButton : String -> Msg -> Element Msg
disabledButton label action =
    Input.button
        [ Background.color <| rgb255 238 238 238
        , Font.color <| rgb255 44 44 44
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


sumTwoWithAce : Rank -> Int
sumTwoWithAce rank =
    {--
    Score rules:
        * If we have an Ace and either 10/J/Q/K, ace counts as 11
        * Otherwise Ace counts as 1 
    --}
    if List.member rank [ Ten, Jack, Queen, King ] then
        21

    else
        rankValue rank + rankValue Ace


calculateScore : List Card -> Int
calculateScore cards =
    let
        ranks =
            List.map (\card -> card.rank) cards
    in
    case ranks of
        Ace :: [ card ] ->
            sumTwoWithAce card

        card :: [ Ace ] ->
            sumTwoWithAce card

        _ ->
            sumByRanks ranks


calculateResultString : List Card -> String
calculateResultString cards =
    if calculateScore cards > 21 then
        "YOU LOSE"

    else if calculateScore cards == 21 then
        "YOU WIN"

    else
        ""


getGameStatus : List Card -> GameStatus
getGameStatus cards =
    if calculateScore cards >= 21 then
        Stopped

    else
        Running



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = mainView
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
