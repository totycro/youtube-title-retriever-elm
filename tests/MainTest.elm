module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (VideoId(..), parseVideoId)
import Test exposing (..)
import Url


suite : Test
suite =
    describe "parseVideoId"
        [ test "parses example with /watch/ and v" <|
            \_ ->
                Expect.equal (parseVideoId "https://www.youtube.com/watch?v=abc") (VideoId "abc" |> Just)
        , test "parses example without /watch/ but v" <|
            \_ ->
                Expect.equal (parseVideoId "https://www.youtube.com/?v=abc") (VideoId "abc" |> Just)
        , test "parses youtu.be link" <|
            \_ ->
                Expect.equal (parseVideoId "https://youtu.be/abc") (VideoId "abc" |> Just)
        , test "failes if param missing" <|
            \_ ->
                Expect.equal (parseVideoId "https://www.youtube.com/watch?other=abc") Nothing
        ]
