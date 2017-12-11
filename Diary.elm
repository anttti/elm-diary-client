module Diary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode exposing (..)


type alias Model =
    { entries : Maybe (List Entry)
    , newEntry : NewEntry
    }


type alias NewEntry =
    { content : String
    }


type alias Entry =
    { id : Int
    , date : String
    , content : String
    }


type Msg
    = HandleFetchResponse (Result Http.Error (List Entry))
    | HandleHTTPError
    | HandleSaveResponse (Result Http.Error Entry)
    | SaveEntry
    | UpdateContent String


initialModel : Model
initialModel =
    { entries = Just []
    , newEntry = { content = "" }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchEntries )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleFetchResponse result ->
            case (Debug.log "handling" result) of
                Ok newEntries ->
                    ( { model | entries = Just newEntries }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        HandleSaveResponse result ->
            case result of
                Ok _ ->
                    ( model, fetchEntries )

                Err _ ->
                    ( model, Cmd.none )

        HandleHTTPError ->
            ( { model | entries = Nothing }, Cmd.none )

        SaveEntry ->
            ( model, saveEntry model.newEntry )

        UpdateContent content ->
            let
                entry =
                    model.newEntry

                newEntry =
                    { entry | content = content }
            in
                ( { model | newEntry = newEntry }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Diary" ]
        , viewEntries model.entries
        , viewForm model.newEntry
        ]


viewEntries : Maybe (List Entry) -> Html msg
viewEntries mentries =
    case mentries of
        Just entries ->
            ul [] (List.map viewEntry entries)

        Nothing ->
            text ""


viewEntry : Entry -> Html msg
viewEntry entry =
    li []
        [ text entry.content
        ]


viewForm : NewEntry -> Html Msg
viewForm newEntry =
    form [ onSubmit SaveEntry ]
        [ textarea [ Html.Attributes.value newEntry.content, onInput UpdateContent ] []
        , input [ type_ "submit", value "Save" ] []
        ]


fetchEntries : Cmd Msg
fetchEntries =
    let
        url =
            "http://localhost:3000/entries"
    in
        Http.get url responseDecoder
            |> Http.send HandleFetchResponse


responseDecoder : Decoder (List Entry)
responseDecoder =
    Json.Decode.list entryDecoder


entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> required "id" Json.Decode.int
        |> required "date" Json.Decode.string
        |> required "content" Json.Decode.string


entryEncoder : NewEntry -> Json.Encode.Value
entryEncoder entry =
    Json.Encode.object
        [ ( "content", Json.Encode.string entry.content )
        ]


saveEntry : NewEntry -> Cmd Msg
saveEntry entry =
    let
        url =
            "http://localhost:3000/entries"

        body =
            entry
                |> entryEncoder
                |> Http.jsonBody
    in
        Http.post url body entryDecoder
            |> Http.send HandleSaveResponse
