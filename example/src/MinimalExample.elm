module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes


-- Import Multiselect

import Multiselect


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Provide values


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


valuesB : List ( String, String )
valuesB =
    [ ( "0", "AAA" )
    , ( "1", "AAB" )
    , ( "2", "ABA" )
    , ( "3", "ABB" )
    , ( "4", "BAA" )
    , ( "5", "BAB" )
    , ( "6", "BBA" )
    , ( "7", "BBB" )
    ]


type alias Flags =
    {}



-- Setup model


type alias Model =
    { multiselectA : Multiselect.Model
    , multiselectB : Multiselect.Model
    }


model : Model
model =
    { multiselectA = Multiselect.initModel valuesA "A"
    , multiselectB = Multiselect.initModel valuesB "B"
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( model, Cmd.none )



-- UPDATE
-- Setup messages


type Msg
    = NoOp
    | HOI Multiselect.Msg
    | Nyan Multiselect.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HOI sub ->
            let
                ( subModel, subCmd ) =
                    Multiselect.update sub model.multiselectA
            in
                { model | multiselectA = subModel } ! [ Cmd.map HOI subCmd ]

        Nyan sub ->
            let
                ( subModel, subCmd ) =
                    Multiselect.update sub model.multiselectB
            in
                { model | multiselectB = subModel } ! [ Cmd.map Nyan subCmd ]



-- VIEW
-- Setup view


view : Model -> Html Msg
view model =
    div []
        [ Html.map HOI <| Multiselect.view model.multiselectA
        , div [ Html.Attributes.style [ ( "height", "300px" ) ] ] [ text "" ]
        , Html.map Nyan <| Multiselect.view model.multiselectB
        ]



-- SUBSCRIPTIONS
-- Setup subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map HOI <| Multiselect.subscriptions model.multiselectA
        , Sub.map Nyan <| Multiselect.subscriptions model.multiselectB
        ]
