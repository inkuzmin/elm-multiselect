module Multiselect exposing
    ( initModel, getSelectedValues, populateValues, clearInputText, getValues
    , Model
    , Msg
    , OutMsg(..)
    , view
    , update
    , subscriptions
    , InputInMenu(..)
    )

{-| An implementation of multiselect control built with and for Elm.

Please, check example/src/MinimalExample.elm for the minimal example on how to use this library.


# Helpers

@docs initModel, getSelectedValues, populateValues, clearInputText, getValues, InputInMenu


# Model

@docs Model


# Msg

@docs Msg


# OutMsg

@docs OutMsg


# View

@docs view


# Update

@docs update


# Subscriptions

@docs subscriptions

-}

import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import DOM
import Html exposing (Html)
import Html.Events as Events
import Html.Styled
    exposing
        ( div
        , span
        , text
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Multiselect.Keycodes as Keycodes
import Multiselect.SelectCss as SelectCss
import Process
import String
import Task as Task exposing (Task)
import Time


type Status
    = Closed
    | Focused
    | Opened


{-| Whether or to show the input field as the first result of the menu or not
useful for tagging
-}
type InputInMenu
    = Show
    | Hide


{-| Opaque type that holds the model

    type alias Model =
        { multiselect : Multiselect.Model
        }

-}
type Model
    = Model
        { status : Status
        , values : List ( String, String )
        , filtered : List ( String, String )
        , selected : List ( String, String )
        , protected : Bool
        , error : Maybe String
        , input : Maybe String
        , inputWidth : Float
        , hovered : Maybe ( String, String )
        , tag : String
        , inputInMenu : InputInMenu
        }


{-| Init model based on the values : List (String, String) and id : String provided by the user.

    model =
        { multiselect = Multiselect.initModel [ ( "one", "The 1st option" ), ( "two", "The 2nd option" ), ( "three", "The 3rd option" ) ] "id_1"
        }

-}
initModel : List ( String, String ) -> String -> InputInMenu -> Model
initModel values tag1 inputInMenu =
    Model
        { status = Closed
        , values = values
        , filtered = values
        , selected = []
        , protected = False
        , error = Nothing
        , input = Just ""
        , inputWidth = 23.0
        , hovered = List.head values
        , tag = tag1
        , inputInMenu = inputInMenu
        }


{-| Get the full list of values : List (String, String)
-}
getValues : Model -> List ( String, String )
getValues (Model model) =
    model.values


{-| Get selected values : List (String, String)
-}
getSelectedValues : Model -> List ( String, String )
getSelectedValues (Model model) =
    model.selected


{-| Populate model with values : List (String, String) and preselect selected : List (String, String).
-}
populateValues : Model -> List ( String, String ) -> List ( String, String ) -> Model
populateValues (Model model) values selected =
    let
        filtered =
            if List.isEmpty selected then
                values

            else
                valuesWithoutSelected { selected = selected, values = values }
    in
    Model { model | values = values, filtered = filtered, selected = selected }


{-| Clear the input text: (Model, Cmd Msg)
-}
clearInputText : Model -> ( Model, Cmd Msg )
clearInputText (Model model) =
    ( Model { model | input = Nothing }
    , Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
    )


valuesWithoutSelected : { selected : List ( String, String ), values : List ( String, String ) } -> List ( String, String )
valuesWithoutSelected { selected, values } =
    List.filter (\value -> not (List.member value selected)) values



-- UPDATE


{-| Opaque type for internal library messages
-}
type Msg
    = Start
    | Click
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
    | OnHover ( String, String )
    | Shortcut Int
    | ScrollY (Result Dom.Error Float)


{-| Transparent type for external library messages
-}
type OutMsg
    = Selected ( String, String )
    | Unselected ( String, String )
    | Cleared
    | NotFound String


{-| Update the control state

    MultiselectMsg subMsg ->
        let
            ( subModel, subCmd ) =
                Multiselect.update subMsg model.multiselect
        in
            { model | multiselect = subModel } ! [ Cmd.map MultiselectMsg subCmd ]

-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg (Model model) =
    case msg of
        Start ->
            ( Model model, Cmd.none, Nothing )

        Toggle ->
            if model.status == Opened then
                ( Model { model | status = Closed }
                , Cmd.batch
                    [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                    ]
                , Nothing
                )

            else
                ( Model { model | status = Opened }
                , Cmd.batch
                    [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                    ]
                , Nothing
                )

        Click ->
            if model.protected then
                ( Model { model | protected = False }, Cmd.none, Nothing )

            else
                ( Model { model | status = Closed }, Cmd.none, Nothing )

        DisableProtection ->
            ( Model { model | protected = False }, Cmd.none, Nothing )

        ClickOnComponent ->
            if model.protected then
                ( Model model, Cmd.none, Nothing )

            else
                ( Model { model | status = Opened, protected = True }
                , Cmd.batch
                    [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                    , delayInMs 100 <| DisableProtection
                    ]
                , Nothing
                )

        ScrollResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( Model { model | error = Just ("Could not find dom id: " ++ id) }, Cmd.none, Nothing )

                Ok () ->
                    case model.input of
                        Nothing ->
                            ( Model { model | input = Just "" }, Cmd.none, Nothing )

                        Just _ ->
                            ( Model { model | error = Nothing }, Cmd.none, Nothing )

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( Model { model | error = Just ("Could not find dom id: " ++ id) }, Cmd.none, Nothing )

                Ok () ->
                    case model.input of
                        Nothing ->
                            ( Model { model | input = Just "" }, Cmd.none, Nothing )

                        Just _ ->
                            ( Model { model | error = Nothing }, Cmd.none, Nothing )

        Adjust value ->
            ( Model { model | inputWidth = value }, Cmd.none, Nothing )

        Filter value ->
            if model.protected then
                ( Model { model | protected = False }, Cmd.none, Nothing )

            else
                let
                    lowerValue =
                        String.toLower value

                    valuesMatchingSearch =
                        model.values
                            |> List.filter (\( _, val ) -> String.contains lowerValue (String.toLower val))

                    maybePrefixedWithValue =
                        if model.inputInMenu == Hide || value == "" || List.any (\( _, val ) -> String.toLower val == value) valuesMatchingSearch then
                            valuesMatchingSearch

                        else
                            ( value, value ) :: valuesMatchingSearch

                    filtered =
                        valuesWithoutSelected
                            { selected = model.selected
                            , values = maybePrefixedWithValue
                            }
                in
                case model.hovered of
                    Nothing ->
                        ( Model
                            { model
                                | filtered = filtered
                                , input = Just value
                                , hovered = List.head filtered
                                , status =
                                    if List.isEmpty filtered then
                                        Closed

                                    else
                                        Opened
                            }
                        , Cmd.none
                        , Nothing
                        )

                    Just item ->
                        if List.length (List.filter (\i -> i == item) filtered) == 0 then
                            ( Model
                                { model
                                    | filtered = filtered
                                    , input = Just value
                                    , hovered = List.head filtered
                                    , status =
                                        if List.isEmpty filtered then
                                            Closed

                                        else
                                            Opened
                                }
                            , Cmd.none
                            , Nothing
                            )

                        else
                            ( Model
                                { model
                                    | filtered = filtered
                                    , input = Just value
                                    , status =
                                        if List.isEmpty filtered then
                                            Closed

                                        else
                                            Opened
                                }
                            , Cmd.none
                            , Nothing
                            )

        OnSelect item ->
            let
                selected =
                    model.selected ++ [ item ]

                filtered =
                    valuesWithoutSelected { selected = selected, values = model.values }
            in
            ( Model
                { model
                    | selected = selected
                    , filtered = filtered
                    , hovered = nextSelectedItem model.filtered item
                    , input = Nothing
                    , status =
                        if List.isEmpty filtered then
                            Closed

                        else
                            Opened
                }
            , Cmd.batch
                [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                ]
            , Just (Selected item)
            )

        RemoveItem item ->
            let
                selected =
                    List.filter (\value -> value /= item) model.selected
            in
            ( Model
                { model
                    | selected = selected
                    , filtered = valuesWithoutSelected { selected = selected, values = model.values }
                    , hovered = Just item
                }
            , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
            , Just (Unselected item)
            )

        Clear ->
            let
                selected =
                    []
            in
            ( Model
                { model
                    | selected = selected
                    , filtered = valuesWithoutSelected { selected = selected, values = model.values }
                    , input = Nothing
                    , status = Closed
                }
            , Cmd.batch
                [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                ]
            , Just Cleared
            )

        OnHover item ->
            ( Model { model | hovered = Just item }, Cmd.none, Nothing )

        ScrollY result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( Model { model | error = Just ("Could not find dom id: " ++ id) }, Cmd.none, Nothing )

                Ok y ->
                    case model.hovered of
                        Nothing ->
                            ( Model model, Cmd.none, Nothing )

                        Just item ->
                            case indexOf item model.filtered of
                                Nothing ->
                                    ( Model model, Cmd.none, Nothing )

                                Just idx ->
                                    let
                                        boundaries =
                                            getBoundaries (toFloat idx)

                                        vpBoundaries =
                                            getViewPortBoundaries y

                                        scroll =
                                            fitViewPort boundaries vpBoundaries
                                    in
                                    ( Model { model | error = Nothing }
                                    , Cmd.batch [ domScrollToY ("multiselectMenu" ++ model.tag) scroll |> Task.attempt ScrollResult ]
                                    , Nothing
                                    )

        Shortcut key ->
            if key == Keycodes.upArrow then
                case model.hovered of
                    Nothing ->
                        ( Model { model | hovered = List.head model.filtered }, Cmd.none, Nothing )

                    Just item ->
                        let
                            prev =
                                prevItem model.filtered item
                        in
                        ( Model { model | hovered = prev }
                        , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                        , Nothing
                        )

            else if key == Keycodes.downArrow then
                case model.hovered of
                    Nothing ->
                        ( Model { model | hovered = List.head model.filtered }, Cmd.none, Nothing )

                    Just item ->
                        let
                            next =
                                nextItem model.filtered item
                        in
                        ( Model { model | hovered = next }
                        , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                        , Nothing
                        )

            else if key == Keycodes.pageUp || key == Keycodes.home then
                let
                    first =
                        List.head model.filtered
                in
                ( Model { model | hovered = first }
                , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                , Nothing
                )

            else if key == Keycodes.pageDown || key == Keycodes.end then
                let
                    last =
                        lastElem model.filtered
                in
                ( Model { model | hovered = last }
                , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                , Nothing
                )

            else if
                key
                    == Keycodes.return
                    && (-- we don't know which item the user is selecting with a closed list, so ignore return
                        model.status
                            == Opened
                            -- except: when the filtered is empty, in which case we assume user wants to add filtered
                            || List.isEmpty model.filtered
                        --
                       )
            then
                case model.hovered of
                    Nothing ->
                        case model.input of
                            Nothing ->
                                ( Model model, Cmd.none, Nothing )

                            Just "" ->
                                ( Model model, Cmd.none, Nothing )

                            Just input_ ->
                                ( Model model, Cmd.none, Just (NotFound input_) )

                    Just ( id, val ) ->
                        let
                            lowerVal =
                                String.toLower val
                        in
                        if model.inputInMenu == Show && Just val == model.input && List.all (\( _, value ) -> String.toLower value /= lowerVal) model.values then
                            ( Model model, Cmd.none, Just (NotFound val) )

                        else
                            let
                                item =
                                    ( id, val )

                                selected =
                                    model.selected ++ [ item ]

                                filtered =
                                    valuesWithoutSelected { selected = selected, values = model.values }
                            in
                            ( Model
                                { model
                                    | selected = selected
                                    , filtered = filtered
                                    , hovered = nextSelectedItem model.filtered item
                                    , input = Nothing
                                    , status =
                                        if List.isEmpty filtered then
                                            Closed

                                        else
                                            Opened
                                }
                            , Cmd.batch
                                [ Dom.focus ("multiselectInput" ++ model.tag) |> Task.attempt FocusResult
                                ]
                            , Just (Selected item)
                            )

            else if key == Keycodes.escape then
                ( Model { model | status = Closed, protected = True }, Cmd.none, Nothing )

            else if key == Keycodes.tab then
                ( Model { model | status = Closed }, Cmd.none, Nothing )

            else if key == Keycodes.backspace then
                case model.input of
                    Just "" ->
                        case lastElem model.selected of
                            Nothing ->
                                ( Model model, Cmd.none, Nothing )

                            Just item ->
                                let
                                    selected =
                                        List.filter (\value -> value /= item) model.selected
                                in
                                ( Model
                                    { model
                                        | selected = selected
                                        , filtered = valuesWithoutSelected { selected = selected, values = model.values }
                                        , hovered = Just item
                                    }
                                , Cmd.batch [ domScrollY ("multiselectMenu" ++ model.tag) |> Task.attempt ScrollY ]
                                , Just (Unselected item)
                                )

                    _ ->
                        ( Model model, Cmd.none, Nothing )

            else
                ( Model model, Cmd.none, Nothing )


getViewPortBoundaries : Float -> ( Float, Float )
getViewPortBoundaries i =
    ( i, i + SelectCss.menuHeight )


getBoundaries : Float -> ( Float, Float )
getBoundaries i =
    ( i * SelectCss.itemHeight, (i * SelectCss.itemHeight) + SelectCss.itemHeight )


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

                _ :: [] ->
                    Nothing

                _ :: y :: _ ->
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


prevItem : List a -> a -> Maybe a
prevItem list item =
    nextItem (List.reverse list) item


delayInMs : Int -> msg -> Cmd msg
delayInMs ms msg =
    -- https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm
    Process.sleep (toFloat ms)
        |> Task.perform (\_ -> msg)


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            Json.Decode.succeed successValue

        Err errorMessage ->
            Json.Decode.fail errorMessage



-- VIEW


onClickNoDefault : msg -> Html.Styled.Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            }
    in
    Html.Styled.Events.custom "click" (withOptions config (Json.Decode.succeed message))


{-| Render the view

    Html.map MultiselectMsg <| Multiselect.view model.multiselect

-}
view : Model -> Html Msg
view =
    styledView >> Html.Styled.toUnstyled


styledView : Model -> Html.Styled.Html Msg
styledView (Model model) =
    let
        inputCss =
            if model.status == Focused then
                [ SelectCss.container, SelectCss.focused ]

            else if model.status == Opened then
                [ SelectCss.container, SelectCss.opened ]

            else
                [ SelectCss.container ]
    in
    div
        [ css [ SelectCss.wrap ]
        , onClick ClickOnComponent
        ]
        [ div
            [ css inputCss
            ]
            [ tags <| Model model
            , input <| Model model
            , clear <| Model model
            , arrow <| Model model
            ]
        , menu <| Model model
        ]


input : Model -> Html.Styled.Html Msg
input (Model model) =
    let
        w =
            String.fromFloat (model.inputWidth + 23.0)

        inputStyle =
            Html.Styled.Attributes.style "width" (w ++ "px")

        forceClear =
            case model.input of
                Nothing ->
                    -- clear input value
                    Html.Styled.Attributes.value ""

                Just _ ->
                    -- no op attribute
                    Html.Styled.Attributes.classList []
    in
    div
        [ preventDefaultButtons
        , css [ SelectCss.inputWrap ]
        ]
        [ div [ css [ SelectCss.inputMirrow ] ] [ text (Maybe.withDefault "" model.input) ]
        , Html.Styled.input
            [ Html.Styled.Attributes.id ("multiselectInput" ++ model.tag)
            , css [ SelectCss.input ]
            , onKeyDown Adjust
            , onKeyPress Shortcut
            , onKeyUp Filter
            , inputStyle
            , Html.Styled.Attributes.property "type" (Encode.string "text")
            , forceClear
            ]
            []
        ]


preventDefaultButtons : Html.Styled.Attribute Msg
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
            Html.Styled.Events.keyCode
                |> Json.Decode.andThen (filterKey >> fromResult)
                |> Json.Decode.map (always Start)
    in
    Html.Styled.Events.custom "keydown" (withOptions options decoder)


onKeyUp : (String -> msg) -> Html.Styled.Attribute msg
onKeyUp tagger =
    Html.Styled.Events.on "keyup" (Json.Decode.map tagger Html.Styled.Events.targetValue)


onKeyDown : (Float -> msg) -> Html.Styled.Attribute msg
onKeyDown tagger =
    let
        domF =
            DOM.target (DOM.previousSibling DOM.offsetWidth)
    in
    Html.Styled.Events.on
        "keypress"
        (Json.Decode.map tagger domF)


onKeyPress : (Int -> msg) -> Html.Styled.Attribute msg
onKeyPress tagger =
    Html.Styled.Events.on "keydown" (Json.Decode.map tagger Html.Styled.Events.keyCode)


tags : Model -> Html.Styled.Html Msg
tags (Model model) =
    div [ css [ SelectCss.tagWrap ] ]
        (List.map
            (\( name, value ) ->
                tag name value
            )
            model.selected
        )


tag : String -> String -> Html.Styled.Html Msg
tag name value =
    div [ css [ SelectCss.tag ] ]
        [ span
            [ css [ SelectCss.tagIcon ]
            , onClick (RemoveItem ( name, value ))
            ]
            [ text "×" ]
        , span [ css [ SelectCss.tagLabel ] ] [ text value ]
        ]


arrow : Model -> Html.Styled.Html Msg
arrow (Model model) =
    let
        arrowCss =
            if model.status == Opened then
                [ SelectCss.arrowUpside ]

            else
                [ SelectCss.arrow ]

        arrowRel =
            if model.status == Opened then
                "arrowUpside"

            else
                "arrow"
    in
    div
        [ css [ SelectCss.arrowWrap ]
        , onClickNoDefault Toggle
        ]
        [ div
            [ css arrowCss
            , Html.Styled.Attributes.rel arrowRel
            ]
            []
        ]


clear : Model -> Html.Styled.Html Msg
clear (Model model) =
    if not (List.isEmpty model.selected) then
        div
            [ css [ SelectCss.clearWrap ]
            , onClickNoDefault Clear
            ]
            [ div [ css [ SelectCss.clear ] ] [ text "×" ] ]

    else
        div [] []


menu : Model -> Html.Styled.Html Msg
menu (Model model) =
    case model.status of
        Opened ->
            let
                hovered =
                    case model.hovered of
                        Nothing ->
                            ""

                        Just ( id, _ ) ->
                            id
            in
            div [ css [ SelectCss.menu ], Html.Styled.Attributes.id ("multiselectMenu" ++ model.tag) ]
                (List.map
                    (\( name, value ) ->
                        div
                            [ css
                                (if name == hovered then
                                    [ SelectCss.menuItemHovered, SelectCss.menuItem ]

                                 else
                                    [ SelectCss.menuItem ]
                                )
                            , onClickNoDefault (OnSelect ( name, value ))
                            , Html.Styled.Events.onMouseOver (OnHover ( name, value ))
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
subscriptions (Model model) =
    if model.status == Opened then
        BrowserEvents.onClick (Json.Decode.succeed Click)

    else
        Sub.none



-- OPERATIONS
-- HTTP
-- HELPER


withOptions : { preventDefault : Bool, stopPropagation : Bool } -> Decoder msg -> Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
withOptions options decoder =
    decoder
        |> Json.Decode.map (\m -> { message = m, preventDefault = options.preventDefault, stopPropagation = options.stopPropagation })


domScrollY : String -> Task Dom.Error Float
domScrollY id =
    Task.map (\vp -> vp.viewport.y) (Dom.getViewportOf id)


domScrollToY : String -> Float -> Task Dom.Error ()
domScrollToY id y =
    Dom.getViewportOf id |> Task.andThen (\vp -> Dom.setViewportOf id vp.viewport.x y)
