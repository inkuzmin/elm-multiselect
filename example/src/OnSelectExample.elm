port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Multiselect exposing (OutMsg(..))


port alert : String -> Cmd msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


valuesA : List ( String, String )
valuesA =
    [ ( "elixir-estonia", "ELIXIR Estonia" )
    , ( "unitartu", "University of Tartu" )
    , ( "cs", "Institute of Computer Science" )
    , ( "biit", "BIIT" )
    , ( "javascript", "JavaScript" )
    , ( "elm", "Elm" )
    , ( "multiselect", "Multiselect" )
    , ( "haskell", "Haskell" )
    , ( "elixir", "Elixir" )
    , ( "clojure", "Clojure" )
    , ( "shen", "Shen" )
    ]


type alias Model =
    { multiselect : Multiselect.Model }


model : Model
model =
    { multiselect = Multiselect.initModel valuesA "A" }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = MultiselectMsg Multiselect.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MultiselectMsg subMsg ->
            let
                ( subModel, subCmd, outMsg ) =
                    Multiselect.update subMsg model.multiselect

                ( outModel, outCmd ) =
                    interpretOutMsg outMsg model

                cmd =
                    Cmd.batch
                        [ Cmd.map MultiselectMsg subCmd
                        , outCmd
                        ]
            in
                ( { model | multiselect = subModel }, cmd )


interpretOutMsg : OutMsg -> Model -> ( Model, Cmd msg )
interpretOutMsg outMsg model =
    case outMsg of
        NoOp ->
            ( model, Cmd.none )

        Select code label ->
            ( model, alert <| "Select " ++ label ++ " (" ++ code ++ ")" )

        Unselect code label ->
            ( model, alert <| "Unselect " ++ label ++ " (" ++ code ++ ")" )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map MultiselectMsg <| Multiselect.view model.multiselect ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MultiselectMsg <| Multiselect.subscriptions model.multiselect
