module Main exposing (Model, Msg(..), VideoId(..), init, main, parseVideoId, subscriptions, update, view)

import Browser
import Config exposing (apiKey)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, index, string)
import List.Extra
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
    { youtubeUrls : String
    , titleData : List ( Maybe VideoId, TitleData )
    }


type VideoId
    = VideoId String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { youtubeUrls = "https://youtube.com/watch/?v=u7SoNJxOVdE"
      , titleData = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlUpdated String
    | UrlSubmitted
    | GotVideoMetadata VideoId (Result Http.Error String)


parseVideoIds : String -> List (Maybe VideoId)
parseVideoIds youtubeUrls =
    let
        lines =
            String.split "\n" youtubeUrls

        cleanLines =
            List.Extra.filterNot String.isEmpty <|
                List.map String.trim lines
    in
    List.map parseVideoId cleanLines


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdated newYoutubeUrls ->
            ( { model | youtubeUrls = newYoutubeUrls }, Cmd.none )

        UrlSubmitted ->
            let
                ( titleData, commands ) =
                    List.unzip <|
                        List.map
                            (\videoId ->
                                case videoId of
                                    Just id ->
                                        ( ( videoId, Loading ), loadVideoMetadata id apiKey )

                                    Nothing ->
                                        ( ( videoId, Failure "Failed to parse youtube url" ), Cmd.none )
                            )
                        <|
                            parseVideoIds model.youtubeUrls
            in
            ( { model | titleData = titleData }, Cmd.batch commands )

        GotVideoMetadata videoId result ->
            let
                -- this is not very FP. change titleData to OrderedDict?
                updateData : TitleData -> List ( Maybe VideoId, TitleData )
                updateData =
                    \newData ->
                        List.Extra.updateIf
                            (\entry -> Tuple.first entry == Just videoId)
                            (always ( Just videoId, newData ))
                            model.titleData
            in
            case result of
                Ok videoTitle ->
                    ( { model | titleData = updateData <| Success videoTitle }, Cmd.none )

                Err error ->
                    ( { model | titleData = updateData <| Failure "http call error" }, Cmd.none )


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
        ([ div [] [ text "Enter the urls: " ]
         , div []
            [ textarea [ cols 80, rows 10, placeholder "url", value model.youtubeUrls, onInput UrlUpdated ] [] ]
         , div
            []
            [ button [ onClick UrlSubmitted ] [ text "Submit" ] ]
         ]
            ++ List.concatMap viewTitle model.titleData
        )


viewTitle : ( Maybe VideoId, TitleData ) -> List (Html Msg)
viewTitle ( videoId, titleData ) =
    let
        videoIdStr =
            Maybe.withDefault "" (Maybe.map (\(VideoId x) -> x) videoId)
    in
    case titleData of
        NotAsked ->
            []

        Loading ->
            [ div [] [ text "Loading" ] ]

        Failure err ->
            [ div [] [ text <| "Error loading title \"" ++ videoIdStr ++ "\": " ++ err ] ]

        Success title ->
            [ div [] [ text ("Title of \"" ++ videoIdStr ++ "\": " ++ title) ] ]



-- HTTP


loadVideoMetadata :
    VideoId
    -> String
    -> Cmd Msg -- TODO: better type for apiKey config?
loadVideoMetadata (VideoId videoId) apiKey =
    Http.get
        { url = "https://www.googleapis.com/youtube/v3/videos?part=snippet&fields=items/snippet/title&id=" ++ videoId ++ "&key=" ++ apiKey
        , expect = Http.expectJson (GotVideoMetadata (VideoId videoId)) videoTitleDecoder
        }


videoTitleDecoder : Decoder String
videoTitleDecoder =
    field "items" <| index 0 <| field "snippet" <| field "title" string
