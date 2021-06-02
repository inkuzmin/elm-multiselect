module DynamicExample exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Http as Http
import Json.Decode as Decode
import Multiselect


main =
    Browser.document
        { init = init
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


valuesC : List ( String, String )
valuesC =
    []


type alias Flags =
    {}


type alias Model =
    { multiselectC : Multiselect.Model
    }


initModel : Model
initModel =
    { multiselectC = Multiselect.initModel valuesC "C" Multiselect.Hide
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, prepopulateValues )



-- UPDATE


type Msg
    = NoOp
    | Yay Multiselect.Msg
    | Prepopulate (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Yay sub ->
            let
                ( subModel, subCmd, maybeOutMsg ) =
                    Multiselect.update sub model.multiselectC
            in
            ( { model | multiselectC = subModel }, Cmd.map Yay subCmd )

        Prepopulate (Ok vs) ->
            let
                multiselectModel =
                    model.multiselectC

                values =
                    List.map (\v -> ( v, v )) vs
            in
            ( { model | multiselectC = Multiselect.populateValues multiselectModel values [] }, Cmd.none )

        Prepopulate (Err _) ->
            Debug.log "error" ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.h3 [] [ text "Contributors (dynamic population of values)" ]
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
        [ Sub.map Yay <| Multiselect.subscriptions model.multiselectC
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



--valuesDecoder : Decode.Decoder


valuesDecoder =
    Decode.field "login" Decode.string


decodeUrl : Decode.Decoder (List String)
decodeUrl =
    Decode.list valuesDecoder
