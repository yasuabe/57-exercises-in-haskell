module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (a, button, div, input, table, td, text, th, tr)
import Html.Attributes exposing (href, placeholder, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Stats =
    { longUrl : String
    , shortUrl : String
    , visitCount : Int
    }


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , input : String
    }


type Page
    = InputPage
    | StatsPage (Result String Stats)


type Msg
    = Input String
    | Submit
    | GotShorten (Result Http.Error String)
    | GotStats (Result Http.Error Stats)
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        parsed =
            parseUrl url
    in
    case parsed of
        Just short ->
            ( { navKey = navKey, page = StatsPage (Err "Loading..."), input = "" }
            , fetchStats short
            )

        Nothing ->
            ( { navKey = navKey, page = InputPage, input = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | input = s }, Cmd.none )

        Submit ->
            ( model, sendShortenRequest model.input )

        GotShorten (Ok short) ->
            ( model, Nav.pushUrl model.navKey ("/ex54/" ++ short ++ "/stats") )

        GotShorten (Err _) ->
            ( { model | page = StatsPage (Err "Shorten failed.") }, Cmd.none )

        GotStats (Ok stats) ->
            ( { model | page = StatsPage (Ok stats) }, Cmd.none )

        GotStats (Err _) ->
            ( { model | page = StatsPage (Err "Failed to fetch stats.") }, Cmd.none )

        UrlChanged url ->
            case parseUrl url of
                Just short ->
                    ( { model | page = StatsPage (Err "Loading...") }, fetchStats short )

                Nothing ->
                    ( { model | page = InputPage }, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        InputPage ->
            { title = "Shorten URL"
            , body =
                [ input [ placeholder "Enter URL", value model.input, onInput Input ] []
                , button [ onClick Submit ] [ text "Shorten" ]
                ]
            }

        StatsPage result ->
            { title = "Stats"
            , body =
                case result of
                    Ok stats ->
                        [ table []
                            [ tr []
                                [ th [] [ text "Short URL" ]
                                , td []
                                    [ a
                                        [ href ("http://localhost:8080/ex54/" ++ stats.shortUrl)
                                        , target "_blank"
                                        ]
                                        [ text stats.shortUrl ]
                                    ]
                                ]
                            , tr []
                                [ th [] [ text "Long URL" ]
                                , td [] [ text stats.longUrl ]
                                ]
                            , tr []
                                [ th [] [ text "Visits" ]
                                , td [] [ text (String.fromInt stats.visitCount) ]
                                ]
                            ]
                        ]

                    Err msg ->
                        [ div [] [ text msg ] ]
            }


route : Parser (String -> a) a
route =
    s "ex54" </> string </> s "stats"


parseUrl : Url -> Maybe String
parseUrl url =
    Parser.parse route url


sendShortenRequest : String -> Cmd Msg
sendShortenRequest input =
    let
        body =
            Http.jsonBody <| Encode.object [ ( "longUrl", Encode.string input ) ]

        decoder =
            Decode.field "shortUrl" Decode.string
    in
    Http.post
        { url = "http://localhost:8080/ex54/"
        , body = body
        , expect = Http.expectJson GotShorten decoder
        }


fetchStats : String -> Cmd Msg
fetchStats code =
    let
        url =
            "http://localhost:8080/ex54/" ++ code ++ "/stats"

        decoder =
            Decode.map3 Stats
                (Decode.field "longUrl" Decode.string)
                (Decode.field "shortUrl" Decode.string)
                (Decode.field "visitCount" Decode.int)
    in
    Http.get { url = url, expect = Http.expectJson GotStats decoder }
