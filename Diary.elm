module Diary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { entries : Maybe (List Entry)
    }


type alias Entry =
    { id : Int
    , date : String
    , content : String
    }


type Msg
    = HandleResponse (Result Http.Error (List Entry))
    | HandleError


initialModel : Model
initialModel =
    { entries =
        Just
            [ Entry 0 "2012-12-11" "This is dummy content 1"
            , Entry 1 "2012-12-12" "This is dummy content 2"
            ]
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchEntries )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleResponse result ->
            case (Debug.log "handling" result) of
                Ok newEntries ->
                    ( { model | entries = Just newEntries }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        HandleError ->
            ( { model | entries = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm Diary" ]
        , viewEntries model.entries
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


fetchEntries : Cmd Msg
fetchEntries =
    let
        url =
            "http://localhost:3000/entries"
    in
        Http.get url responseDecoder
            |> Http.send HandleResponse


responseDecoder : Decoder (List Entry)
responseDecoder =
    Json.Decode.list entryDecoder


entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> required "id" Json.Decode.int
        |> required "date" Json.Decode.string
        |> required "content" Json.Decode.string
