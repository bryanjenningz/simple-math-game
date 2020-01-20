module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, input, li, ol, progress, text)
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onClick, onInput)
import Random
import Task
import Time



---- MODEL ----


type alias Model =
    { page : Page
    , name : String
    , hiscores : List Hiscore
    }


type Page
    = MainPage
    | GamePage GameInfo
    | EnteringHiscorePage Hiscore


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
    ( { page = MainPage, name = "", hiscores = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = GameStarted
    | StartTimeGenerated Int
    | FirstQuestionGenerated Int Question
    | QuestionGenerated Question
    | EndTimeGenerated Int
    | SetAnswerString String
    | SubmitAnswer
    | SetName String
    | SubmitName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStarted ->
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

        SetAnswerString answerString ->
            case model.page of
                GamePage gameInfo ->
                    ( { model | page = GamePage { gameInfo | answerString = answerString } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitAnswer ->
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
                            EnteringHiscorePage
                                { name = model.name, totalTimeSeconds = endTime // 1000 - gameInfo.gameStartUnixTime }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetName name ->
            case model.page of
                EnteringHiscorePage hiscore ->
                    ( { model | page = EnteringHiscorePage { hiscore | name = name } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitName ->
            case model.page of
                EnteringHiscorePage hiscore ->
                    ( { model
                        | page = MainPage
                        , hiscores = List.sortBy .totalTimeSeconds (hiscore :: model.hiscores)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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
            (Random.int 100 999)
            (Random.int 100 999)
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
                , button [ onClick GameStarted ] [ text "Start Game!" ]
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
                    , input [ onInput SetAnswerString, value gameInfo.answerString ] []
                    ]
                , button [ onClick SubmitAnswer ] [ text "Submit answer" ]
                ]

        EnteringHiscorePage hiscore ->
            div []
                [ h2 [] [ text "New hiscore!" ]
                , input [ value hiscore.name, onInput SetName ] []
                , button [ onClick SubmitName ] [ text "Add name to hiscores!" ]
                ]


viewProgress : Int -> Html msg
viewProgress questionsCompleted =
    progress [ attribute "max" "100", value (String.fromFloat <| toFloat questionsCompleted / 5 * 100) ] []


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
