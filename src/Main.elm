module Main exposing (..)

import Array
import Browser exposing (Document)
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, b)
import Html.Events
import Json.Decode as Decode
import Random
import Time


main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions model =
    Time.every 1000 Tick


type Mode
    = Additions
    | Multiplications
    | Subtractions
    | Divisions
    | Mixed


type Model
    = InGame InGameModel
    | Welcome
    | GameOver Score


type alias InGameModel =
    { mode : Mode
    , question : Question
    , text : String
    , score : Int
    , remainingSeconds : Int
    }


type alias Score =
    Int


type Msg
    = Submit
    | ChangeText String
    | NewQuestion Question
    | Tick Time.Posix
    | StartGame Mode
    | MainMenu


type Operation
    = Add
    | Sub
    | Mul
    | Div


type alias Question =
    { operation : Operation
    , left : Int
    , right : Int
    , result : Int
    }


buildQuestion : Operation -> Int -> Int -> Question
buildQuestion op a b =
    case op of
        Add ->
            { operation = op, left = a, right = b, result = a + b }

        Sub ->
            let
                first =
                    max a b

                second =
                    min a b
            in
            { operation = op, left = first, right = second, result = first - second }

        Mul ->
            { operation = op, left = a, right = b, result = a * b }

        Div ->
            { operation = op, left = a * b, right = b, result = a }


defaultQuestion : Question
defaultQuestion =
    { operation = Add
    , left = 0
    , right = 0
    , result = 0
    }


generateQuestion mode =
    Random.generate NewQuestion (randomQuestion mode)


randomQuestion mode =
    case mode of
        Additions ->
            Random.map2 (buildQuestion Add) (Random.int 0 100) (Random.int 0 100)

        Subtractions ->
            Random.map2 (buildQuestion Sub) (Random.int 0 100) (Random.int 0 100)

        Multiplications ->
            Random.map2 (buildQuestion Mul) (Random.int 1 10) (Random.int 1 10)

        Divisions ->
            Random.map2 (buildQuestion Div) (Random.int 1 10) (Random.int 1 10)

        Mixed ->
            Random.int 1 4
                |> Random.andThen
                    (\num ->
                        case num of
                            1 ->
                                randomQuestion Additions

                            2 ->
                                randomQuestion Subtractions

                            3 ->
                                randomQuestion Multiplications

                            _ ->
                                randomQuestion Divisions
                    )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Welcome, Cmd.none )


startGame : Mode -> ( Model, Cmd Msg )
startGame mode =
    ( InGame
        { question = defaultQuestion
        , score = 0
        , mode = mode
        , remainingSeconds = 60
        , text = ""
        }
    , generateQuestion mode
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InGame m ->
            updateInGame msg m

        Welcome ->
            updateWelcome msg

        GameOver score ->
            updateGameOver msg score


updateInGame : Msg -> InGameModel -> ( Model, Cmd Msg )
updateInGame msg model =
    case msg of
        Submit ->
            case String.toInt model.text of
                Nothing ->
                    ( InGame model, Cmd.none )

                Just result ->
                    if result == model.question.result then
                        ( InGame { model | score = model.score + 1, text = "" }, generateQuestion model.mode )

                    else
                        ( InGame
                            { model
                                | score = reduceScore model.score
                                , text = ""
                            }
                        , generateQuestion model.mode
                        )

        ChangeText newText ->
            ( InGame { model | text = newText }, Cmd.none )

        NewQuestion question ->
            ( InGame { model | question = question }
            , Cmd.none
            )

        Tick _ ->
            if model.remainingSeconds == 0 then
                ( GameOver model.score, Cmd.none )

            else
                ( InGame { model | remainingSeconds = model.remainingSeconds - 1 }, Cmd.none )

        StartGame mode ->
            -- shouldn't be happening
            startGame mode

        MainMenu ->
            ( Welcome, Cmd.none )


reduceScore score =
    if score - 1 <= 0 then
        0

    else
        score - 1


updateWelcome : Msg -> ( Model, Cmd Msg )
updateWelcome msg =
    case msg of
        StartGame mode ->
            startGame mode

        _ ->
            ( Welcome, Cmd.none )


updateGameOver : Msg -> Score -> ( Model, Cmd Msg )
updateGameOver msg score =
    case msg of
        StartGame mode ->
            startGame mode

        MainMenu ->
            ( Welcome, Cmd.none )

        _ ->
            ( GameOver score, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Mathe-Übung"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Element.layout [] <|
        case model of
            Welcome ->
                viewWelcome

            InGame inGameModel ->
                viewInGame inGameModel

            GameOver score ->
                viewGameOver score


viewAttributes =
    [ centerX
    , centerY
    , spacing 30
    , padding 30
    , Border.rounded 4
    , Border.width 1
    , Border.color darkGreen
    , Background.color lightGreen
    , width (px 400)
    ]


viewWelcome =
    column
        viewAttributes
        [ el [ centerX, Font.size 50 ] (text "Willkommen!")
        , text "Lust auf Mathe?"
        , startButton "Plus" Additions
        , startButton "Minus" Subtractions
        , startButton "Mal" Multiplications
        , startButton "Geteilt" Divisions
        , startButton "Durcheinander" Mixed
        ]


viewGameOver score =
    let
        ( pic, message ) =
            gameOverMessage score
    in
    column
        viewAttributes
        [ el [ centerX, Font.size 50 ] (text <| String.fromInt score ++ " Punkte")
        , text message
        , text "Schaffst du noch mehr?"
        , menuButton "Nochmal"
        ]


gameOverMessage score =
    if score < 10 then
        ( "😐", "Nächstes Mal wird's bestimmt besser!" )

    else if score < 20 then
        ( "🙂", "Gut gemacht!" )

    else if score < 30 then
        ( "😎", "Mega!" )

    else if score < 40 then
        ( "😍", "Superduper!" )

    else if score < 50 then
        ( "🥳", "Juhu! Das war ja fantastisch!" )

    else
        ( "🦖", "Uuuuuaaaa! Ich bin ein T. Rex!" )


startButton label mode =
    Input.button
        [ Background.color darkGreen
        , Font.color <| rgb255 255 255 255
        , Border.rounded 4
        , padding 10
        , centerX
        ]
        { onPress = Just <| StartGame mode, label = text label }


menuButton label =
    Input.button
        [ Background.color darkGreen
        , Font.color <| rgb255 255 255 255
        , Border.rounded 4
        , padding 10
        , centerX
        ]
        { onPress = Just MainMenu, label = text label }


darkRed =
    rgb255 128 0 0


darkGreen =
    rgb255 0 128 0


lightGreen =
    rgb255 220 255 220


viewInGame : InGameModel -> Element Msg
viewInGame { question, score, text, remainingSeconds } =
    column
        viewAttributes
        [ topBar score remainingSeconds
        , questionLine question
        , inputBox text
        , answerButton
        ]


topBar score seconds =
    row [ width fill, spacing 30 ]
        [ el [] (text <| String.fromInt seconds ++ "s")
        , el [ alignRight ] (text <| String.fromInt score ++ " P")
        ]


operatorString op =
    case op of
        Add ->
            " + "

        Sub ->
            " - "

        Mul ->
            " · "

        Div ->
            " : "


questionLine { operation, left, right } =
    el [ centerX ] (text <| String.fromInt left ++ operatorString operation ++ String.fromInt right)


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


inputBox text =
    Input.text [ Input.focusedOnLoad, onEnter Submit ]
        { label = Input.labelHidden "Ergebnis"
        , onChange = ChangeText
        , text = text
        , placeholder = Nothing
        }


answerButton =
    Input.button
        [ Background.color darkGreen
        , Font.color <| rgb255 255 255 255
        , Border.rounded 4
        , padding 10
        ]
        { onPress = Just Submit, label = text "Überprüfen" }
