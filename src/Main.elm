module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode
import LineChart exposing (view1)



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
    { date : String
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
        (Decode.field "date" Decode.string)
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
            div [] [ countryChart data.us ]



-- countyChart


datumElement : CoronaDatum -> Html msg
datumElement datum =
    div []
        [ text datum.date
        , text <| String.fromInt datum.confirmed
        , text <| String.fromInt datum.deaths
        , text <| String.fromInt datum.recovered
        ]


dataForCountry : List CoronaDatum -> Html msg
dataForCountry listData =
    div [] <|
        List.map
            datumElement
            listData



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
