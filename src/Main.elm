module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, input, label, li, ol, progress, text)
import Html.Attributes exposing (attribute, class, value)
import Html.Events exposing (onClick, onInput)
import Random
import Task
import Time



---- MODEL ----


type alias Model =
    { page : Page
    , name : String
    , hiscores : List Hiscore
    , currentTime : Int
    }


type Page
    = MainPage
    | GamePage GameInfo
    | HiscoreEntryPage Hiscore


type alias GameInfo =
    { questionsCompleted : Int
    , gameStartUnixTime : Int
    , question : Question
    , answerString : String
    }


type alias Question =
    { first : Int
    , second : Int
    , operator : Operator
    }


type Operator
    = Add
    | Subtract


type alias Hiscore =
    { name : String
    , totalTimeSeconds : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { page = MainPage, name = "", currentTime = 0, hiscores = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = StartGameButtonClicked
    | StartTimeGenerated Int
    | FirstQuestionGenerated Int Question
    | QuestionGenerated Question
    | EndTimeGenerated Int
    | AnswerInputChanged String
    | SubmitAnswerButtonClicked
    | HiscoreNameInputChanged String
    | HiscoreSubmitNameButtonClicked
    | GotCurrentTime Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGameButtonClicked ->
            ( model, Task.perform (Time.posixToMillis >> StartTimeGenerated) Time.now )

        StartTimeGenerated startTime ->
            ( model, generateQuestion (FirstQuestionGenerated (startTime // 1000)) )

        FirstQuestionGenerated startTime question ->
            ( { model
                | page =
                    GamePage
                        { questionsCompleted = 0
                        , gameStartUnixTime = startTime
                        , question = question
                        , answerString = ""
                        }
              }
            , Cmd.none
            )

        QuestionGenerated question ->
            case model.page of
                GamePage gameInfo ->
                    ( { model | page = GamePage { gameInfo | question = question } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AnswerInputChanged answerString ->
            case model.page of
                GamePage gameInfo ->
                    ( { model | page = GamePage { gameInfo | answerString = answerString } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitAnswerButtonClicked ->
            case model.page of
                GamePage gameInfo ->
                    case String.toInt gameInfo.answerString of
                        Just answer ->
                            if operatorFunction gameInfo.question.operator gameInfo.question.first gameInfo.question.second == answer then
                                if gameInfo.questionsCompleted + 1 == 5 then
                                    ( model, Task.perform (Time.posixToMillis >> EndTimeGenerated) Time.now )

                                else
                                    ( { model
                                        | page =
                                            GamePage
                                                { gameInfo
                                                    | questionsCompleted = gameInfo.questionsCompleted + 1
                                                    , answerString = ""
                                                }
                                      }
                                    , generateQuestion QuestionGenerated
                                    )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EndTimeGenerated endTime ->
            case model.page of
                GamePage gameInfo ->
                    ( { model
                        | page =
                            HiscoreEntryPage
                                { name = model.name, totalTimeSeconds = endTime // 1000 - gameInfo.gameStartUnixTime }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        HiscoreNameInputChanged name ->
            case model.page of
                HiscoreEntryPage hiscore ->
                    ( { model | page = HiscoreEntryPage { hiscore | name = name }, name = name }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HiscoreSubmitNameButtonClicked ->
            case model.page of
                HiscoreEntryPage hiscore ->
                    ( { model
                        | page = MainPage
                        , hiscores = List.sortBy .totalTimeSeconds (hiscore :: model.hiscores)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotCurrentTime currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )


operatorFunction : Operator -> Int -> Int -> Int
operatorFunction operator =
    case operator of
        Add ->
            (+)

        Subtract ->
            (-)


generateQuestion : (Question -> Msg) -> Cmd Msg
generateQuestion toMsg =
    Random.generate toMsg <|
        Random.map3 Question
            (Random.int 1 20)
            (Random.int 1 20)
            (Random.uniform Add [ Subtract ])



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.page of
        MainPage ->
            div []
                [ h1 [] [ text "Math Game" ]
                , if List.isEmpty model.hiscores then
                    text ""

                  else
                    ol [] (List.map viewHiscore model.hiscores)
                , button [ class "btn", onClick StartGameButtonClicked ] [ text "Start Game!" ]
                ]

        GamePage gameInfo ->
            div []
                [ viewProgress gameInfo.questionsCompleted
                , h2 []
                    [ text <|
                        String.fromInt gameInfo.question.first
                            ++ " "
                            ++ operatorToString gameInfo.question.operator
                            ++ " "
                            ++ String.fromInt gameInfo.question.second
                            ++ " = "
                    , input [ onInput AnswerInputChanged, value gameInfo.answerString ] []
                    ]
                , button [ class "btn", onClick SubmitAnswerButtonClicked ] [ text "Submit answer" ]
                , viewGameTime (model.currentTime - gameInfo.gameStartUnixTime)
                ]

        HiscoreEntryPage hiscore ->
            div []
                [ h2 [] [ text "New hiscore!" ]
                , div [] [ text ("You finished in " ++ String.fromInt hiscore.totalTimeSeconds ++ " seconds!") ]
                , label [] [ text "Enter your name", input [ class "input", value hiscore.name, onInput HiscoreNameInputChanged ] [] ]
                , button [ class "btn", onClick HiscoreSubmitNameButtonClicked ] [ text "Done!" ]
                ]


viewProgress : Int -> Html msg
viewProgress questionsCompleted =
    progress [ attribute "max" "100", value (String.fromFloat <| toFloat questionsCompleted / 5 * 100) ] []


viewGameTime : Int -> Html msg
viewGameTime time =
    div [ class "game-time" ] [ text <| String.fromInt time ++ " seconds" ]


viewHiscore : Hiscore -> Html msg
viewHiscore hiscore =
    li []
        [ text <|
            "Name: "
                ++ hiscore.name
                ++ ", Finish time: "
                ++ String.fromInt hiscore.totalTimeSeconds
                ++ " seconds"
        ]


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        Add ->
            "+"

        Subtract ->
            "-"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 (Time.posixToMillis >> (\millis -> millis // 1000) >> GotCurrentTime)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
