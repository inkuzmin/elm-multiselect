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


type alias Flags =
    {}


type alias Model =
    { multiselectA : Multiselect.Model
    , multiselectB : Multiselect.Model
    }


model : Model
model =
    { multiselectA = Multiselect.initModel "A"
    , multiselectB = Multiselect.initModel "B"
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( model, Cmd.none )



-- UPDATE


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


view : Model -> Html Msg
view model =
    div []
        [ Html.map HOI <| Multiselect.view model.multiselectA
        , div [ Html.Attributes.style [ ( "height", "300px" ) ] ] [ text "" ]
        , Html.map Nyan <| Multiselect.view model.multiselectB
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map HOI <| Multiselect.subscriptions model.multiselectA
        , Sub.map Nyan <| Multiselect.subscriptions model.multiselectB
        ]
