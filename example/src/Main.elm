module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Multiselect
import Mouse
import Html.Attributes
import Json.Decode as Decode
import Http


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


valuesC : List ( String, String )
valuesC =
    []


type alias Flags =
    {}


type alias Model =
    { multiselectA : Multiselect.Model
    , multiselectB : Multiselect.Model
    , multiselectC : Multiselect.Model
    , selectedA : List ( String, String )
    }


model : Model
model =
    { multiselectA = Multiselect.initModel valuesA "A"
    , multiselectB = Multiselect.initModel valuesB "B"
    , multiselectC = Multiselect.initModel valuesC "C"
    , selectedA = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( model, prepopulateValues )



-- UPDATE


type Msg
    = NoOp
    | HOI Multiselect.Msg
    | Nyan Multiselect.Msg
    | Yay Multiselect.Msg
    | SelectA
    | Prepopulate (Result Http.Error (List String))


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

        Yay sub ->
            let
                ( subModel, subCmd ) =
                    Multiselect.update sub model.multiselectC
            in
                { model | multiselectC = subModel } ! [ Cmd.map Yay subCmd ]

        SelectA ->
            ( { model | selectedA = Multiselect.getSelectedValues model.multiselectA }, Cmd.none )

        Prepopulate (Ok vs) ->
            let
                multiselectModel =
                    model.multiselectC

                values =
                    List.map (\v -> ( v, v )) vs
            in
                { model | multiselectC = Multiselect.populateValues multiselectModel values [] } ! []

        Prepopulate (Err _) ->
            Debug.log "error" ( model, Cmd.none )



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
        , div [ Html.Attributes.style [ ( "height", "300px" ) ] ] [ text "" ]
        , Html.h3 [] [ text "Contributors (dynamic fetching of values)" ]
        , Html.map Yay <| Multiselect.view model.multiselectC
        , showSelected (Multiselect.getSelectedValues model.multiselectC)
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
        , Sub.map Yay <| Multiselect.subscriptions model.multiselectC
        ]



-- HELPERS


prepopulateValues =
    let
        url =
            "https://api.github.com/repos/inkuzmin/elm-multiselect/contributors"

        request =
            Http.get url decodeUrl
    in
        Http.send Prepopulate request


decodeUrl : Decode.Decoder (List String)
decodeUrl =
    Decode.list (Decode.field "login" Decode.string)
