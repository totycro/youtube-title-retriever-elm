module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Config exposing (apiKey)
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, index, string)
import RemoteData exposing (RemoteData(..), WebData)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { youtubeUrl : String
    , titleData : WebData String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { youtubeUrl = "u7SoNJxOVdE", titleData = NotAsked }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlUpdated String
    | UrlSubmitted
    | GotVideoMetadata (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdated newYoutubeUrl ->
            ( { model | youtubeUrl = newYoutubeUrl }, Cmd.none )

        UrlSubmitted ->
            ( model, loadVideoMetadata model.youtubeUrl apiKey )

        -- TODO: for now just use interpret full url as video id, later parse video id from url
        GotVideoMetadata result ->
            case result of
                Ok videoTitle ->
                    ( { model | titleData = Success videoTitle }, Cmd.none )

                Err error ->
                    ( { model | titleData = Failure error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ div []
            [ text "Enter the url: "
            , input [ placeholder "url", value model.youtubeUrl, onInput UrlUpdated ] []
            , button [ onClick UrlSubmitted ] [ text "Submit" ]
            ]
         ]
            ++ viewTitle model.titleData
        )


viewTitle : WebData String -> List (Html Msg)
viewTitle titleData =
    case titleData of
        NotAsked ->
            []

        Loading ->
            [ text "Loading" ]

        Failure err ->
            [ text "Error loading title" ]

        Success title ->
            [ text ("Title: " ++ title) ]



-- HTTP


loadVideoMetadata :
    String
    -> String
    -> Cmd Msg -- TODO: bad type signature
loadVideoMetadata videoId apiKey =
    Http.get
        { url = "https://www.googleapis.com/youtube/v3/videos?part=snippet&fields=items/snippet/title&id=" ++ videoId ++ "&key=" ++ apiKey, expect = Http.expectJson GotVideoMetadata videoTitleDecoder }


videoTitleDecoder : Decoder String
videoTitleDecoder =
    field "items" (index 0 (field "snippet" (field "title" string)))
