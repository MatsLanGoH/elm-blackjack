module Main exposing (..)

import Browser
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , px
        , rgb
        , rgb255
        , row
        , spacing
        , text
        , width
        )
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


type GameResult
    = Draw
    | PlayerWins
    | DealerWins


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
                {- Temporary: Player, Dealer both draw at the same time -}
                newModel =
                    drawCard Player model
                        |> drawCard Dealer
            in
            ( newModel |> updateGameStatus
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
                |> dealInitialHand
            , Cmd.none
            )

        RestartGame ->
            ( model, createShuffledStack )

        NoOp ->
            ( model, Cmd.none )



--- UPDATE HELPERS ---


drawCard : PlayerType -> Model -> Model
drawCard playerType model =
    let
        drawnCard =
            List.take 1 model.stackedCards

        newStack =
            List.drop 1 model.stackedCards
    in
    if playerType == Player then
        { model
            | playerHand = model.playerHand ++ drawnCard
            , stackedCards = newStack
        }

    else
        { model
            | dealerHand = model.dealerHand ++ drawnCard
            , stackedCards = newStack
        }


dealInitialHand : Model -> Model
dealInitialHand model =
    drawCard Player model
        |> drawCard Dealer
        |> drawCard Player
        |> drawCard Dealer


updateGameStatus : Model -> Model
updateGameStatus model =
    { model | gameStatus = getGameStatus model }



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
        , Font.color <| rgb 1 1 1
        ]
        [ column
            [ alignLeft
            , width <| fill
            ]
            [ text <| "Remaining cards: " ++ String.fromInt (List.length model.stackedCards)
            ]
        , column
            [ height <| fill
            , width <| fill
            ]
            [ text <| gameResultView model ]
        ]


gameProgressView : Model -> PlayerType -> Element msg
gameProgressView model playertype =
    viewStatus model playertype


gameResultView : Model -> String
gameResultView model =
    if model.gameStatus == Running then
        ""

    else
        "Result: "
            ++ (case getGameResult model of
                    Draw ->
                        "Draw"

                    PlayerWins ->
                        "You win!"

                    DealerWins ->
                        "Dealer wins!"
               )


viewStatus : Model -> PlayerType -> Element msg
viewStatus model playerType =
    let
        viewHand =
            if playerType == Player then
                viewPlayerHand model.playerHand

            else
                viewDealerHand model.dealerHand model.gameStatus
    in
    row
        [ width <| fill
        , height <| px 150
        , padding 10
        , Background.color <| rgb255 30 222 30
        ]
        viewHand


viewPlayerHand : List Card -> List (Element msg)
viewPlayerHand hand =
    [ column
        [ alignLeft
        , Font.color <| rgb 1 1 1
        , width <| fill
        , height <| fill
        ]
        [ el [ alignTop ] (text <| "Player Hand")
        , cardsView hand
        ]
    , viewScore hand
    ]


viewDealerHand : List Card -> GameStatus -> List (Element msg)
viewDealerHand hand gamestatus =
    let
        dealerScore =
            if gamestatus == Running then
                none

            else
                viewScore hand
    in
    [ column
        [ alignLeft
        , Font.color <| rgb 1 1 1
        , width <| fill
        , height <| fill
        ]
        [ el [ alignTop ] (text <| "Dealer Hand")
        , dealerCardsView hand gamestatus
        ]
    , dealerScore
    ]


viewScore : List Card -> Element msg
viewScore hand =
    column
        [ alignRight ]
        [ row []
            [ text "Current Score: "
            , text <| String.fromInt <| calculateScore hand
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


dealerCardsView : List Card -> GameStatus -> Element msg
dealerCardsView cards gamestatus =
    if gamestatus == Stopped then
        cardsView cards

    else
        let
            holeCard =
                List.take 1 cards

            otherCards =
                List.drop 1 cards
        in
        row
            [ Background.color <| rgb 1 1 1
            ]
        <|
            List.concat
                [ List.map holeCardItem holeCard
                , List.map cardItem otherCards
                ]


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


holeCardItem : Card -> Element msg
holeCardItem card =
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
        (text <| holeCardString)


holeCardString : String
holeCardString =
    0x0001F0A0 |> Char.fromCode |> String.fromChar


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



--- RESULT CALCULATION ---


handIsOver21 : List Card -> Bool
handIsOver21 cards =
    calculateScore cards > 21


handIs21 : List Card -> Bool
handIs21 cards =
    calculateScore cards == 21


getGameResult : Model -> GameResult
getGameResult model =
    {- FIXME: 両方とも21未満のまま終わったときのケース -}
    let
        playerWins =
            handIs21 model.playerHand
                || (not <| handIsOver21 model.playerHand)

        dealerWins =
            handIs21 model.dealerHand
                || (not <| handIsOver21 model.dealerHand)
    in
    case ( playerWins, dealerWins ) of
        ( False, False ) ->
            Draw

        ( True, True ) ->
            Draw

        ( True, False ) ->
            PlayerWins

        ( False, True ) ->
            DealerWins


getGameStatus : Model -> GameStatus
getGameStatus model =
    if calculateScore model.playerHand >= 21 then
        Stopped

    else if calculateScore model.dealerHand >= 21 then
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
