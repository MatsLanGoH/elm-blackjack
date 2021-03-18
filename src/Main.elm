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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



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
            , Font.color <| rgb 1 1 0
            ]
            (text "Player Hand")
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = mainView
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
