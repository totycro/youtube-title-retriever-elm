module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Config exposing (apiKey)
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, index, string)



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
    , videoTitle : Maybe String
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { youtubeUrl = "u7SoNJxOVdE", videoTitle = Nothing, error = Nothing }
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
                    ( { model | videoTitle = Just videoTitle }, Cmd.none )

                Err error ->
                    ( { model | error = Just "Loading url failed" }, Cmd.none )



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
            ++ viewTitle model.videoTitle
            ++ viewError model.error
        )


viewTitle : Maybe String -> List (Html Msg)
viewTitle =
    maybeMapDefault []
        (\titleStr -> [ text ("Title:" ++ titleStr) ])


viewError : Maybe String -> List (Html Msg)
viewError =
    maybeMapDefault []
        (\msg -> [ text ("Error: " ++ msg) ])


{-| Borrowed from elm-maybe-extra, which doesn't seem to be installble in this environment
-}
maybeMapDefault : b -> (a -> b) -> Maybe a -> b
maybeMapDefault d f m =
    Maybe.withDefault d (Maybe.map f m)



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
