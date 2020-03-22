module Main exposing (..)

import Browser
import DateHelpers
import Duration
import Html exposing (Html, div, h1, text)
import Http
import Iso8601
import Json.Decode as Decode
import LineChart exposing (view1)
import Svg exposing (Svg)
import Time



---- MODEL ----


type alias Model =
    { data : Maybe CoronaData
    }


init : ( Model, Cmd Msg )
init =
    ( { data = Nothing }, getCoronaData )


type alias CoronaData =
    { us : List CoronaDatum
    , italy : List CoronaDatum
    , china : List CoronaDatum
    }


type alias CoronaDatum =
    { date : Time.Posix
    , confirmed : Int
    , deaths : Int
    , recovered : Int
    }



---- UPDATE ----


type Msg
    = GotCoronaData (Result Http.Error CoronaData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCoronaData result ->
            case result of
                Err error ->
                    case error of
                        Http.BadBody e ->
                            ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Ok value ->
                    ( { model | data = Just value }, Cmd.none )


getCoronaData : Cmd Msg
getCoronaData =
    Http.get
        { url = "https://pomber.github.io/covid19/timeseries.json"
        , expect = Http.expectJson GotCoronaData coronaDataDecoder
        }


coronaDataDecoder : Decode.Decoder CoronaData
coronaDataDecoder =
    Decode.map3 CoronaData
        (Decode.field "US" (Decode.list dayDataDecoder))
        (Decode.field "Italy" (Decode.list dayDataDecoder))
        (Decode.field "China" (Decode.list dayDataDecoder))


dayDataDecoder : Decode.Decoder CoronaDatum
dayDataDecoder =
    Decode.map4 CoronaDatum
        (Decode.field "date" DateHelpers.dateDecoder)
        -- (Decode.field "date" Iso8601.decoder)
        (Decode.field "confirmed" Decode.int)
        (Decode.field "deaths" Decode.int)
        (Decode.field "recovered" Decode.int)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        maybeData =
            model.data
    in
    case maybeData of
        Nothing ->
            div [] [ text "loading" ]

        Just data ->
            div []
                [ countryChart data.us "United States"
                , countryChart data.italy "Italy"
                , countryChart data.china "China"
                ]


countryChart : List CoronaDatum -> String -> Svg msg
countryChart data l =
    div []
        [ h1 [] [ text l ]
        , LineChart.view1 .x .y (List.map coronaDatumToPoint data)
        ]


coronaDatumToPoint : CoronaDatum -> Point
coronaDatumToPoint datum =
    let
        firstDay =
            Iso8601.toTime "2020-01-21"

        days =
            case firstDay of
                Err e ->
                    1

                Ok day ->
                    Duration.inDays <| Duration.from day datum.date
    in
    { x = days, y = toFloat datum.confirmed }


type alias Point =
    { x : Float
    , y : Float
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
