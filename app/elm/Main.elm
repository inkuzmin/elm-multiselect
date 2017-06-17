module Main exposing (..)

import DOM exposing (..)
import Dom
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Mouse
import Process
import SelectCss
import String
import Task
import Time


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


values : List ( String, String )
values =
    [ ( "elixir-estonia", "ELIXIR Estonia" )
    , ( "unitartu", "University of Tartu" )
    , ( "cs", "Institute of Computer Science" )
    , ( "biit", "BIIT" )
    , ( "javascript", "JavaScript" )
    , ( "elm", "Elm" )
    , ( "haskell", "Haskell" )
    ]


type Status
    = Closed
    | Focused
    | Opened
    | Disabled


invisibleCharacter =
    "\x200C\x200C"


type alias Model =
    { status : Status
    , values : List ( String, String )
    , filtered : List ( String, String )
    , selected : List ( String, String )
    , protected : Bool
    , error : Maybe String
    , input : String
    , inputWidth : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model Closed values (sortValues values) [] False Nothing "" 23.0, Cmd.none )


sortValues : List ( String, String ) -> List ( String, String )
sortValues =
    List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2))



-- UPDATE


type Msg
    = Start
    | Click Mouse.Position
    | ClickOnComponent
    | DisableProtection
    | Toggle
    | OnSelect ( String, String )
    | RemoveItem ( String, String )
    | Clear
    | FocusResult (Result Dom.Error ())
    | Filter String
    | Adjust Float
    | ClearInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, Cmd.none )

        Toggle ->
            if model.status == Opened then
                ( { model | status = Closed }, Cmd.none )
            else
                ( { model | status = Opened }, Cmd.none )

        Click _ ->
            if model.protected then
                ( { model | protected = False }, Cmd.none )
            else
                ( { model | status = Closed }, Cmd.none )

        DisableProtection ->
            ( { model | protected = False }, Cmd.none )

        ClickOnComponent ->
            if model.protected then
                ( model, Cmd.none )
            else
                { model | status = Opened, protected = True }
                    ! [ Dom.focus "multiselectInput" |> Task.attempt FocusResult
                      , delay (Time.millisecond * 100) <| DisableProtection
                      ]

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    { model | error = Just ("Could not find dom id: " ++ id) } ! []

                Ok () ->
                    if model.input == invisibleCharacter then
                        { model | input = "" } ! []
                    else
                        { model | error = Nothing } ! []

        ClearInput ->
            { model | input = "" } ! []

        Adjust value ->
            { model | inputWidth = value } ! []

        Filter value ->
            let
                v =
                    List.filter (\v -> not (List.member v model.selected))
                        (List.filter (\( name, val ) -> String.contains (String.toLower value) (String.toLower val)) model.values)
            in
            { model
                | filtered = v
                , input = value
                , status =
                    if List.isEmpty v then
                        Closed
                    else
                        Opened
            }
                ! []

        OnSelect item ->
            let
                v =
                    List.filter (\v -> not (List.member v model.selected))
                        (List.filter (\value -> value /= item) model.values)
            in
            { model
                | selected = model.selected ++ [ item ]
                , filtered = v
                , input = invisibleCharacter
                , status =
                    if List.isEmpty v then
                        Closed
                    else
                        Opened
            }
                ! [ Dom.focus "multiselectInput" |> Task.attempt FocusResult
                  ]

        RemoveItem item ->
            ( { model
                | selected = List.filter (\value -> value /= item) model.selected
                , filtered = List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2)) (item :: model.filtered)
              }
            , Cmd.none
            )

        Clear ->
            { model
                | filtered = sortValues model.values
                , selected = []
                , input = invisibleCharacter
            }
                ! [ Dom.focus "multiselectInput" |> Task.attempt FocusResult
                  ]


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    -- https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm
    Process.sleep time
        |> Task.perform (\_ -> msg)



-- VIEW


infixr 5 :>
(:>) : (a -> b) -> a -> b
(:>) f x =
    f x


{ id, class, classList } =
    Html.CssHelpers.withNamespace "multiselect"


onClickNoDefault : msg -> Html.Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            }
    in
    Html.Events.onWithOptions "click" config (Json.Decode.succeed message)


view : Model -> Html Msg
view model =
    let
        inputClasses =
            if model.status == Focused then
                [ SelectCss.Container, SelectCss.Focused ]
            else if model.status == Opened then
                [ SelectCss.Container, SelectCss.Opened ]
            else
                [ SelectCss.Container ]
    in
    div
        [ class [ SelectCss.Wrap ]
        , onClick ClickOnComponent
        ]
        [ div
            [ class inputClasses
            ]
            [ tags model
            , input model
            , clear model
            , arrow model
            ]
        , menu model
        ]


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


input : Model -> Html Msg
input model =
    let
        w =
            toString (model.inputWidth + 23.0)

        inputStyle =
            Html.Attributes.style [ ( "width", w ++ "px" ) ]

        value =
            if model.input == invisibleCharacter then
                Html.Attributes.property "value" (Encode.string model.input)
            else
                Html.Attributes.property "type" (Encode.string "text")
    in
    div [ class [ SelectCss.InputWrap ] ]
        [ div [ class [ SelectCss.InputMirrow ] ] [ text model.input ]
        , Html.input
            [ id "multiselectInput"
            , class [ SelectCss.Input ]
            , onKeyUp Filter
            , onKeyPress Adjust
            , inputStyle
            , value
            ]
            []
        ]


onKeyUp : (String -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.Decode.map tagger Html.Events.targetValue)


onKeyPress : (Float -> msg) -> Html.Attribute msg
onKeyPress tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger (DOM.target :> DOM.previousSibling :> DOM.offsetWidth))


tags : Model -> Html Msg
tags model =
    div [ class [ SelectCss.TagWrap ] ]
        (List.map
            (\( name, value ) ->
                tag name value
            )
            model.selected
        )


tag : String -> String -> Html Msg
tag name value =
    div [ class [ SelectCss.Tag ] ]
        [ Html.span
            [ class [ SelectCss.TagIcon ]
            , onClick (RemoveItem ( name, value ))
            ]
            [ text "×" ]
        , Html.span [ class [ SelectCss.TagLabel ] ] [ text value ]
        ]


arrow : Model -> Html Msg
arrow model =
    let
        arrowClasses =
            if model.status == Opened then
                [ SelectCss.ArrowUpside ]
            else
                [ SelectCss.Arrow ]
    in
    div
        [ class [ SelectCss.ArrowWrap ]
        , onClickNoDefault Toggle
        ]
        [ div [ class arrowClasses ] [] ]


clear : Model -> Html Msg
clear model =
    if not (List.isEmpty model.selected) then
        div
            [ class [ SelectCss.ClearWrap ]
            , onClick Clear
            ]
            [ div [ class [ SelectCss.Clear ] ] [ text "×" ] ]
    else
        div [] []


menu : Model -> Html Msg
menu model =
    case model.status of
        Opened ->
            div [ class [ SelectCss.Menu ] ]
                (List.map
                    (\( name, value ) ->
                        div
                            [ class [ SelectCss.MenuItem ]
                            , onClickNoDefault (OnSelect ( name, value ))
                            ]
                            [ text value ]
                    )
                    model.filtered
                )

        _ ->
            div [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == Opened then
        Mouse.clicks Click
    else
        Sub.none



-- OPERATIONS
-- HTTP
