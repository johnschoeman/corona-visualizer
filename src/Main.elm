module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode



---- MODEL ----


type alias Model =
    { data : Maybe CoronaData }


init : ( Model, Cmd Msg )
init =
    ( { data = Nothing }, getCoronaData )


type alias CoronaData =
    { us : List DayData
    }


type alias DayData =
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
    Decode.map CoronaData
        (Decode.field "US" (Decode.list dayDataDecoder))


dayDataDecoder : Decode.Decoder DayData
dayDataDecoder =
    Decode.map4 DayData
        (Decode.field "date" Decode.string)
        (Decode.field "confirmed" Decode.int)
        (Decode.field "deaths" Decode.int)
        (Decode.field "recovered" Decode.int)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
