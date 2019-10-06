module Main exposing (Model, Msg(..), VideoId(..), init, main, parseVideoId, subscriptions, update, view)

import Browser
import Config exposing (apiKey)
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, index, string)
import Maybe.Extra exposing (orElseLazy)
import RemoteData exposing (RemoteData(..))
import Url
import Url.Parser exposing ((<?>), query, s, top)
import Url.Parser.Query



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias TitleData =
    RemoteData String String


type alias Model =
    { youtubeUrl : String
    , titleData : TitleData
    }


type VideoId
    = VideoId String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { youtubeUrl = "https://youtube.com/watch/?v=u7SoNJxOVdE"
      , titleData = NotAsked
      }
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
            let
                videoId =
                    parseVideoId model.youtubeUrl
            in
            case videoId of
                Just id ->
                    ( { model | titleData = Loading }, loadVideoMetadata id apiKey )

                Nothing ->
                    ( { model | titleData = Failure "Failed to parse youtube url" }, Cmd.none )

        GotVideoMetadata result ->
            case result of
                Ok videoTitle ->
                    ( { model | titleData = Success videoTitle }, Cmd.none )

                Err error ->
                    ( { model | titleData = Failure "http call error" }, Cmd.none )


collapseMaybe : Maybe (Maybe a) -> Maybe a
collapseMaybe =
    Maybe.andThen identity


firstMatch : List (a -> Maybe b) -> a -> Maybe b
firstMatch transformers data =
    case transformers of
        [] ->
            Nothing

        transformer :: xs ->
            transformer data |> orElseLazy (\_ -> firstMatch xs data)


parseVideoId : String -> Maybe VideoId
parseVideoId youtubeUrl =
    let
        parsedUrl =
            Url.fromString youtubeUrl

        parsers =
            [ Url.Parser.parse (query (Url.Parser.Query.string "v")) >> collapseMaybe
            , Url.Parser.parse (s "watch" <?> Url.Parser.Query.string "v") >> collapseMaybe
            , \url ->
                -- youtu.be urls just use their path directly. we don't just want to match anything,
                -- so check the host here
                if url.host == "youtu.be" then
                    Url.Parser.parse Url.Parser.string url

                else
                    Nothing
            ]
    in
    parsedUrl
        |> Maybe.andThen (firstMatch parsers)
        |> Maybe.map VideoId



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


viewTitle : TitleData -> List (Html Msg)
viewTitle titleData =
    case titleData of
        NotAsked ->
            []

        Loading ->
            [ text "Loading" ]

        Failure err ->
            [ text <| "Error loading title: " ++ err ]

        Success title ->
            [ text ("Title: " ++ title) ]



-- HTTP


loadVideoMetadata :
    VideoId
    -> String
    -> Cmd Msg -- TODO: better type for apiKey config?
loadVideoMetadata (VideoId videoId) apiKey =
    Http.get
        { url = "https://www.googleapis.com/youtube/v3/videos?part=snippet&fields=items/snippet/title&id=" ++ videoId ++ "&key=" ++ apiKey, expect = Http.expectJson GotVideoMetadata videoTitleDecoder }


videoTitleDecoder : Decoder String
videoTitleDecoder =
    field "items" <| index 0 <| field "snippet" <| field "title" string
