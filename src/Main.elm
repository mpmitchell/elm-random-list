port module Main exposing (Item, Model, Msg(..), editItem, init, main, onEnter, removeItem, setStorage, update, updateWithStorage, view, viewListItem)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random
import Task



---- MODEL ----


type alias Model =
    { field : String
    , items : Array Item
    , selected : Maybe Int
    }


type alias Item =
    { text : String
    , editing : Bool
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault
        (Model "" Array.empty Nothing)
        savedModel
    , Task.attempt (\_ -> NoOp) (Dom.focus "field")
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateField String
    | AddItem
    | RemoveItem Int
    | StartEditing Int
    | UpdateItem Int String
    | EndEditing Int
    | Select
    | Selected Int
    | Clear


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( model_, cmd ) =
            update msg model
    in
    ( model_
    , Cmd.batch
        [ setStorage model_, cmd ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField field ->
            ( { model | field = field }
            , Cmd.none
            )

        AddItem ->
            ( { model
                | field = ""
                , items =
                    if String.isEmpty model.field then
                        model.items

                    else
                        Array.push (Item model.field False) model.items
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "field")
            )

        RemoveItem index ->
            ( removeItem index model
            , Cmd.none
            )

        StartEditing index ->
            ( { model
                | items =
                    editItem
                        index
                        (\item -> { item | editing = True })
                        model.items
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "editing")
            )

        UpdateItem index field ->
            ( { model
                | items =
                    editItem
                        index
                        (\item -> { item | text = field })
                        model.items
              }
            , Cmd.none
            )

        EndEditing index ->
            case Array.get index model.items of
                Nothing ->
                    ( model, Cmd.none )

                Just item ->
                    if String.isEmpty item.text then
                        ( removeItem index model
                        , Cmd.none
                        )

                    else
                        ( { model
                            | items =
                                editItem
                                    index
                                    (\item_ -> { item_ | editing = False })
                                    model.items
                          }
                        , Cmd.none
                        )

        Select ->
            ( model
            , Random.generate Selected
                (Random.int 0 (Array.length model.items - 1))
            )

        Selected index ->
            ( { model
                | selected = Just index
              }
            , Cmd.none
            )

        Clear ->
            ( { model
                | items = Array.empty
                , selected = Nothing
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "field")
            )


editItem : Int -> (Item -> Item) -> Array Item -> Array Item
editItem index function items =
    let
        item =
            Array.get index items
    in
    case item of
        Nothing ->
            items

        Just item_ ->
            Array.set index (function item_) items


removeItem : Int -> Model -> Model
removeItem index model =
    let
        head =
            Array.slice 0 index model.items

        tail =
            Array.slice (index + 1) (Array.length model.items) model.items

        items =
            Array.append head tail
    in
    { model
        | items = items
        , selected =
            case model.selected of
                Nothing ->
                    model.selected

                Just index_ ->
                    if index_ == index then
                        Nothing

                    else
                        model.selected
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "body" ]
        [ div []
            [ input
                [ onInput UpdateField
                , onEnter AddItem
                , value model.field
                , id "field"
                ]
                []
            , button [ onClick AddItem, class "green" ] [ text "Add" ]
            ]
        , ul [] (Array.toList (Array.indexedMap viewListItem model.items))
        , div []
            [ button [ onClick Select ] [ text "Select" ]
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        , case model.selected of
            Nothing ->
                div [] []

            Just index ->
                case Array.get index model.items of
                    Nothing ->
                        div [] []

                    Just item ->
                        div [ id "selected" ] [ text item.text ]
        ]


viewListItem : Int -> Item -> Html Msg
viewListItem index item =
    li []
        [ if not item.editing then
            span
                [ onClick (StartEditing index) ]
                [ text item.text ]

          else
            input
                [ onInput (UpdateItem index)
                , onBlur (EndEditing index)
                , onEnter (EndEditing index)
                , value item.text
                , id "editing"
                ]
                []
        , button [ onClick (RemoveItem index), class "red" ] [ text "X" ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    on "keydown"
        (Json.map
            (\code ->
                if code == 13 then
                    msg

                else
                    NoOp
            )
            keyCode
        )



---- PROGRAM ----


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg
