module Main exposing (..)

import Browser exposing (Document)
import Element
    exposing
        ( Color
        , Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , fromRgb
        , mouseDown
        , padding
        , px
        , rgb
        , rgb255
        , row
        , shrink
        , spacing
        , text
        , toRgb
        , width
        )
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
    , feedback : Feedback
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
        , feedback = SyntaxError
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
                    ( InGame { model | feedback = SyntaxError }, Cmd.none )

                Just result ->
                    if result == model.question.result then
                        ( InGame
                            { model
                                | score = model.score + 1
                                , text = ""
                                , feedback = Correct
                            }
                        , generateQuestion model.mode
                        )

                    else
                        ( InGame
                            { model
                                | score = reduceScore model.score
                                , text = ""
                                , feedback = Incorrect
                            }
                        , generateQuestion model.mode
                        )

        ChangeText newText ->
            if newText == "" && model.text == "" then
                ( InGame model, Cmd.none )

            else
                ( InGame
                    { model
                        | text = newText
                        , feedback =
                            if newText == "" then
                                model.feedback

                            else if String.toInt newText == Nothing then
                                SyntaxError

                            else
                                Ready
                    }
                , Cmd.none
                )

        NewQuestion question ->
            ( InGame { model | question = question, text = "" }
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


viewAttributes baseColor =
    [ centerX
    , centerY
    , spacing 30
    , padding 30
    , Border.rounded 4
    , Border.width 4
    , Border.color <| dark <| baseColor
    , Background.color <| pale <| baseColor
    , width (px 400)
    ]


viewWelcome =
    column
        (viewAttributes <| rgb 0.5 0.5 0.5)
        [ el [ centerX, Font.size 50 ] (text "Hallo Arri!")
        , text "Lust auf Mathe?"
        , row [ Element.width fill, spacing 30 ]
            [ startButton "Plus ➕" Additions
            , startButton "Minus ➖" Subtractions
            ]
        , row [ Element.width fill, spacing 30 ]
            [ startButton "Mal ✖️" Multiplications
            , startButton "Geteilt ➗" Divisions
            ]
        , startButton "Durcheinander ❔" Mixed
        ]


viewGameOver score =
    let
        ( pic, message ) =
            gameOverMessage score
    in
    column
        (viewAttributes <| rgb 0.5 0.5 0.5)
        [ el [ centerX, Font.size 50 ] (text <| String.fromInt score ++ " Punkte")
        , text message
        , el [ Font.size 300 ] <| text pic
        , text "Schaffst du noch mehr?"
        , el [ width shrink, centerX ] <| menuButton "Menü"
        ]


gameOverMessage score =
    if score < 4 then
        ( "😐", "Nächstes Mal wird's bestimmt besser!" )

    else if score < 8 then
        ( "🙂", "Gut gemacht!" )

    else if score < 12 then
        ( "😎", "Mega!" )

    else if score < 16 then
        ( "😍", "Superduper!" )

    else if score < 20 then
        ( "🥳", "Juhu! Das war ja fantastisch!" )

    else
        ( "🦖", "Uuuuuaaaa! Ich bin ein T. Rex!" )


startButton label mode =
    button (modeColor mode) label (Just <| StartGame mode)


menuButton label =
    button greenColor label (Just MainMenu)


button : Color -> String -> Maybe msg -> Element msg
button color label action =
    Input.button
        [ Background.color <| pale color
        , Border.color <| dark color
        , Font.color <| rgb 0 0 0
        , Border.rounded 4
        , Border.width 4
        , padding 10
        , centerX
        , Element.width fill
        , Font.center
        , mouseDown [ Background.color <| dark color, Font.color <| rgb 1 1 1 ]
        ]
        { onPress = action, label = text label }


redColor =
    rgb 1 0 0


greenColor =
    rgb 0 1 0


blueColor =
    rgb 0 1 1


yellowColor =
    rgb 1 1 0


purpleColor =
    rgb 0.5 0 1


grayColor =
    rgb 0.5 0.5 0.5


modeColor : Mode -> Color
modeColor mode =
    case mode of
        Additions ->
            yellowColor

        Subtractions ->
            redColor

        Multiplications ->
            blueColor

        Divisions ->
            greenColor

        Mixed ->
            purpleColor


pale : Color -> Color
pale arg =
    let
        { red, green, blue } =
            toRgb arg

        whiten =
            \c -> 0.25 * c + 0.75
    in
    fromRgb
        { red = whiten red
        , green = whiten green
        , blue = whiten blue
        , alpha = 1.0
        }


dark : Color -> Color
dark arg =
    let
        { red, green, blue } =
            toRgb arg
    in
    fromRgb
        { red = 0.5 * red
        , green = 0.5 * green
        , blue = 0.5 * blue
        , alpha = 1.0
        }


viewInGame : InGameModel -> Element Msg
viewInGame { question, score, text, remainingSeconds, mode, feedback } =
    column
        (viewAttributes <| modeColor mode)
        [ topBar score remainingSeconds
        , questionLine question
        , inputBox text
        , answerButton feedback
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


type Feedback
    = Correct
    | Incorrect
    | SyntaxError
    | Ready


answerButton feedback =
    let
        ( color, feedbackText ) =
            case feedback of
                Correct ->
                    ( greenColor, "Richtig!" )

                Incorrect ->
                    ( redColor, "Falsch!" )

                Ready ->
                    ( grayColor, "Prüfen" )

                SyntaxError ->
                    ( grayColor, "Zahl eingeben" )

        onPress =
            if feedback == Ready then
                Just Submit

            else
                Nothing
    in
    el [ width shrink, centerX ] <| button color feedbackText onPress
