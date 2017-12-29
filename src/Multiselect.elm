module Multiselect exposing (Model, initModel, Msg, update, view, subscriptions, getSelectedValues, populateValues)

{-| An implementation of multiselect control built with and for Elm.

Please, check example/src/MinimalExample.elm for the minimal example on how to use this library.


# Helpers

@docs initModel, getSelectedValues, populateValues


# Model

@docs Model


# Msg

@docs Msg


# View

@docs view


# Update

@docs update


# Subscriptions

@docs subscriptions

-}

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
import Multiselect.SelectCss as SelectCss
import String
import Task
import Time
import Dom.Scroll
import Multiselect.Keycodes as Keycodes
import Multiselect.Utils exposing (fst, snd, invisibleCharacter)


{-| Example values, also used as default ones to make it simpler to start using this library.
-}
values : List ( String, String )
values =
    [ ( "one", "The first option" )
    , ( "two", "The second option" )
    , ( "three", "The third option" )
    , ( "four", "The 4th option" )
    , ( "five", "The 5th option" )
    , ( "six", "The 6th option" )
    , ( "seven", "The 7th option" )
    , ( "eight", "The 8th option" )
    , ( "nine", "The 9th option" )
    , ( "ten", "The 10th option" )
    , ( "eleven", "The 11th option" )
    , ( "twelve", "The 12th option" )
    ]



-- INIT


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


type Status
    = Closed
    | Focused
    | Opened
    | Disabled


{-| Opaque type that holds the model

    type alias Model =
        { multiselect : Multiselect.Model
        }

-}
type alias Model =
    { status : Status
    , values : List ( String, String )
    , filtered : List ( String, String )
    , selected : List ( String, String )
    , protected : Bool
    , error : Maybe String
    , input : String
    , inputWidth : Float
    , hovered : Maybe ( String, String )
    , tag : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel values "A"
    , Cmd.none
    )


{-| Init model based on the values : List (String, String) and id : String provided by the user.

    model =
        { multiselect = Multiselect.initModel [ ( "one", "The 1st option" ), ( "two", "The 2nd option" ), ( "three", "The 3rd option" ) ] "id_1"
        }

-}
initModel : List ( String, String ) -> String -> Model
initModel values tag =
    Model
        Closed
        values
        values
        []
        False
        Nothing
        ""
        23.0
        (List.head values)
        tag


{-| Get selected values : List (String, String)
-}
getSelectedValues : Model -> List ( String, String )
getSelectedValues model =
    model.selected


{-| Populate model with values : List (String, String) and preselect selected : List (String, String).
-}
populateValues : Model -> List ( String, String ) -> List ( String, String ) -> Model
populateValues model values selected =
    let
        filtered =
            if (List.isEmpty selected) then
                values
            else
                filter selected values
    in
        { model | values = values, filtered = filtered, selected = selected }


filter : List ( String, String ) -> List ( String, String ) -> List ( String, String )
filter selected values =
    List.filter (\value -> not (List.member value selected)) values



-- UPDATE


{-| Opaque type for internal library messages
-}
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
    | ScrollResult (Result Dom.Error ())
    | Filter String
    | Adjust Float
    | ClearInput
    | OnHover ( String, String )
    | Shortcut Int
    | ScrollY (Result Dom.Error Float)


{-| Update the control state

    MultiselectMsg subMsg ->
        let
            ( subModel, subCmd ) =
                Multiselect.update subMsg model.multiselect
        in
            { model | multiselect = subModel } ! [ Cmd.map MultiselectMsg subCmd ]

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, Cmd.none )

        Toggle ->
            if model.status == Opened then
                ({ model | status = Closed }
                    ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                      ]
                )
            else
                ({ model | status = Opened }
                    ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                      ]
                )

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
                    ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                      , delay (Time.millisecond * 100) <| DisableProtection
                      ]

        ScrollResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    { model | error = Just ("Could not find dom id: " ++ id) } ! []

                Ok () ->
                    if model.input == invisibleCharacter then
                        { model | input = "" } ! []
                    else
                        { model | error = Nothing } ! []

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
            ({ model | input = "" } ! [])

        Adjust value ->
            ({ model | inputWidth = value } ! [])

        Filter value ->
            let
                filtered =
                    filter model.selected
                        (List.filter (\( name, val ) -> String.contains (String.toLower value) (String.toLower val))
                            model.values
                        )
            in
                if model.protected then
                    ( { model | protected = False }, Cmd.none )
                else
                    case model.hovered of
                        Nothing ->
                            { model
                                | filtered = filtered
                                , input = value
                                , hovered = List.head filtered
                                , status =
                                    if List.isEmpty filtered then
                                        Closed
                                    else
                                        Opened
                            }
                                ! []

                        Just item ->
                            if List.length (List.filter (\i -> i == item) filtered) == 0 then
                                { model
                                    | filtered = filtered
                                    , input = value
                                    , hovered = List.head filtered
                                    , status =
                                        if List.isEmpty filtered then
                                            Closed
                                        else
                                            Opened
                                }
                                    ! []
                            else
                                { model
                                    | filtered = filtered
                                    , input = value
                                    , status =
                                        if List.isEmpty filtered then
                                            Closed
                                        else
                                            Opened
                                }
                                    ! []

        OnSelect item ->
            let
                selected =
                    model.selected ++ [ item ]

                filtered =
                    filter selected model.values
            in
                { model
                    | selected = selected
                    , filtered = filtered
                    , hovered = nextSelectedItem model.filtered item
                    , input = invisibleCharacter
                    , status =
                        if List.isEmpty filtered then
                            Closed
                        else
                            Opened
                }
                    ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                      ]

        RemoveItem item ->
            let
                selected =
                    List.filter (\value -> value /= item) model.selected
            in
                { model
                    | selected = selected
                    , filtered = filter selected model.values
                    , hovered = Just item
                }
                    ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]

        Clear ->
            let
                selected =
                    []
            in
                { model
                    | selected = selected
                    , filtered = filter selected model.values
                    , input = invisibleCharacter
                    , status = Closed
                }
                    ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                      ]

        OnHover item ->
            { model | hovered = Just item } ! []

        ScrollY result ->
            case result of
                Err (Dom.NotFound id) ->
                    { model | error = Just ("Could not find dom id: " ++ id) } ! []

                Ok y ->
                    case model.hovered of
                        Nothing ->
                            model ! []

                        Just item ->
                            case indexOf item model.filtered of
                                Nothing ->
                                    model ! []

                                Just idx ->
                                    let
                                        boundaries =
                                            getBoundaries (toFloat idx)

                                        vpBoundaries =
                                            getViewPortBoundaries y

                                        scroll =
                                            fitViewPort boundaries vpBoundaries
                                    in
                                        { model | error = Nothing }
                                            ! [ Dom.Scroll.toY ("multiselectMenu" ++ model.tag) scroll |> Task.attempt ScrollResult ]

        Shortcut key ->
            if key == Keycodes.upArrow then
                case model.hovered of
                    Nothing ->
                        { model | hovered = List.head model.filtered } ! []

                    Just item ->
                        let
                            prev =
                                prevItem (model.filtered) item
                        in
                            { model | hovered = prev }
                                ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
            else if key == Keycodes.downArrow then
                case model.hovered of
                    Nothing ->
                        { model | hovered = List.head model.filtered } ! []

                    Just item ->
                        let
                            next =
                                nextItem (model.filtered) item
                        in
                            { model | hovered = next }
                                ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
            else if key == Keycodes.pageUp || key == Keycodes.home then
                let
                    first =
                        List.head (model.filtered)
                in
                    { model | hovered = first }
                        ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
            else if key == Keycodes.pageDown || key == Keycodes.end then
                let
                    last =
                        lastElem (model.filtered)
                in
                    { model | hovered = last }
                        ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
            else if key == Keycodes.return then
                case model.hovered of
                    Nothing ->
                        model ! []

                    Just item ->
                        let
                            selected =
                                model.selected ++ [ item ]

                            filtered =
                                filter selected model.values
                        in
                            { model
                                | selected = selected
                                , filtered = filtered
                                , hovered = nextSelectedItem model.filtered item
                                , input = invisibleCharacter
                                , status =
                                    if List.isEmpty filtered then
                                        Closed
                                    else
                                        Opened
                            }
                                ! [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                                  ]
            else if key == Keycodes.escape || key == Keycodes.tab then
                { model | status = Closed, protected = True } ! []
            else if key == Keycodes.backspace then
                if model.input == "" then
                    case lastElem model.selected of
                        Nothing ->
                            model ! []

                        Just item ->
                            let
                                selected =
                                    List.filter (\value -> value /= item) model.selected
                            in
                                { model
                                    | selected = selected
                                    , filtered = filter selected model.values
                                    , hovered = Just item
                                }
                                    ! [ Dom.Scroll.y ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                else
                    model ! []
            else
                model ! []


getViewPortBoundaries : Float -> ( Float, Float )
getViewPortBoundaries i =
    ( i, i + SelectCss.menuHeight )


getBoundaries : Float -> ( Float, Float )
getBoundaries i =
    ( (i * SelectCss.itemHeight), (i * SelectCss.itemHeight) + SelectCss.itemHeight )


fitViewPort : ( Float, Float ) -> ( Float, Float ) -> Float
fitViewPort ( top, bottom ) ( vpTop, vpBottom ) =
    if top < vpTop then
        top
    else if bottom > vpBottom then
        vpTop + (bottom - vpBottom)
    else
        vpTop


indexOf : a -> List a -> Maybe Int
indexOf el list =
    let
        helper l index =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if x == el then
                        Just index
                    else
                        helper xs (index + 1)
    in
        helper list 0


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing


nextSelectedItem : List a -> a -> Maybe a
nextSelectedItem list item =
    let
        takeLast l =
            case l of
                [] ->
                    Nothing

                x :: [] ->
                    Nothing

                x :: y :: rest ->
                    Just y

        findNextInList l =
            case l of
                [] ->
                    Nothing

                x :: [] ->
                    if x == item then
                        takeLast (List.reverse list)
                    else
                        Nothing

                x :: y :: rest ->
                    if x == item then
                        Just y
                    else
                        findNextInList (y :: rest)
    in
        findNextInList list


nextItem : List a -> a -> Maybe a
nextItem list item =
    let
        findNextInList l =
            case l of
                [] ->
                    Nothing

                x :: [] ->
                    if x == item then
                        List.head list
                    else
                        Nothing

                x :: y :: rest ->
                    if x == item then
                        Just y
                    else
                        findNextInList (y :: rest)
    in
        findNextInList list


prevItem list item =
    nextItem (List.reverse list) item


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    -- https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm
    Process.sleep time
        |> Task.perform (\_ -> msg)


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            Json.Decode.succeed successValue

        Err errorMessage ->
            Json.Decode.fail errorMessage



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


{-| Render the view

    Html.map MultiselectMsg <| Multiselect.view model.multiselect

-}
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
        div
            [ preventDefaultButtons
            , class [ SelectCss.InputWrap ]
            ]
            [ div [ class [ SelectCss.InputMirrow ] ] [ text model.input ]
            , Html.input
                [ id ("multiselectInput" ++ model.tag)
                , class [ SelectCss.Input ]
                , onKeyDown Adjust
                , onKeyPress Shortcut
                , onKeyUp Filter
                , inputStyle
                , value
                ]
                []
            ]


preventDefaultButtons : Html.Attribute Msg
preventDefaultButtons =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        filterKey code =
            if code == Keycodes.upArrow || code == Keycodes.downArrow then
                Ok code
            else
                Err "ignored input"

        decoder =
            Html.Events.keyCode
                |> Json.Decode.andThen (filterKey >> fromResult)
                |> Json.Decode.map (always Start)
    in
        Html.Events.onWithOptions "keydown" options decoder


onKeyUp : (String -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.Decode.map tagger Html.Events.targetValue)


onKeyDown : (Float -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Html.Events.on
        "keypress"
        (Json.Decode.map tagger (DOM.target :> DOM.previousSibling :> DOM.offsetWidth))


onKeyPress : (Int -> msg) -> Html.Attribute msg
onKeyPress tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger Html.Events.keyCode)


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
            , onClickNoDefault Clear
            ]
            [ div [ class [ SelectCss.Clear ] ] [ text "×" ] ]
    else
        div [] []


menu : Model -> Html Msg
menu model =
    case model.status of
        Opened ->
            let
                hovered =
                    case model.hovered of
                        Nothing ->
                            ""

                        Just item ->
                            fst item
            in
                div [ class [ SelectCss.Menu ], id ("multiselectMenu" ++ model.tag) ]
                    (List.map
                        (\( name, value ) ->
                            div
                                [ class
                                    (if name == hovered then
                                        [ SelectCss.MenuItemHovered, SelectCss.MenuItem ]
                                     else
                                        [ SelectCss.MenuItem ]
                                    )
                                , onClickNoDefault (OnSelect ( name, value ))
                                , Html.Events.onMouseOver (OnHover ( name, value ))
                                ]
                                [ text value ]
                        )
                        model.filtered
                    )

        _ ->
            div [] []



-- SUBSCRIPTIONS


{-| Subscribe for messages

    Sub.map MultiselectMsg <| Multiselect.subscriptions model.multiselect

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == Opened then
        Mouse.clicks Click
    else
        Sub.none



-- OPERATIONS
-- HTTP
