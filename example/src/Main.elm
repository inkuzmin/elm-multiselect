module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Multiselect
import Mouse
import Html.Attributes


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


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


type alias Model =
    { multiselectA : Multiselect.Model
    , multiselectB : Multiselect.Model
    , selectedA : List ( String, String )
    }


model : Model
model =
    { multiselectA = Multiselect.initModel valuesA "A"
    , multiselectB = Multiselect.initModel valuesB "B"
    , selectedA = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | HOI Multiselect.Msg
    | Nyan Multiselect.Msg
    | SelectA


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

        SelectA ->
            ( { model | selectedA = Multiselect.getSelectedValues model.multiselectA }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.h3 [] [ text "Submit on button click" ]
        , Html.map HOI <| Multiselect.view model.multiselectA
        , showSelected model.selectedA
        , Html.button [ Html.Attributes.class "btn", onClick SelectA ] [ text "Select!" ]
        , div [ Html.Attributes.style [ ( "height", "300px" ) ] ] [ text "" ]
        , Html.h3 [] [ text "Submit on select" ]
        , Html.map Nyan <| Multiselect.view model.multiselectB
        , showSelected (Multiselect.getSelectedValues model.multiselectB)
        ]


showSelected : List ( String, String ) -> Html Msg
showSelected values =
    Html.ul [] (List.map (\( name, value ) -> Html.li [] [ text value ]) values)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map HOI <| Multiselect.subscriptions model.multiselectA
        , Sub.map Nyan <| Multiselect.subscriptions model.multiselectB
        ]
