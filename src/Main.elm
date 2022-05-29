port module Main exposing (main)

import Browser
import Html exposing (Html, input, span, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onBlur, onInput)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map, map2, map7, string)
import List.FlatMap exposing (flatMap)
import Regex



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- PORTS


port clipboard : (String -> msg) -> Sub msg


port updateTags : List String -> Cmd msg



-- MODEL


type KotuStatus
    = Failure
    | Loading
    | Success (List KotuComponent)


type alias KotuSentence =
    { accentPhrases : List KotuAccentPhrase
    }


type alias KotuAccentPhrase =
    { components : List KotuComponent
    }


type alias KotuPitchAccent =
    { descriptive : String
    , mora : Int
    }


type alias KotuComponent =
    { surface : String
    , kana : String
    , surfaceOriginal : String
    , originalKana : String
    , pitchAccent : List KotuPitchAccent
    , isBasic : Bool
    , partOfSpeech : String
    }


type alias Flags =
    { tags : List String
    }


type alias Model =
    { tags : List String
    , tagsInput : String
    , kotuStatus : KotuStatus
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { kotuStatus = Success []
      , tags = flags.tags
      , tagsInput = String.join " " flags.tags
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateTags
    | UpdateTagsInput String
    | ClipboardUpdated String
    | GotKotuResponse (Result Http.Error (List KotuSentence))


spacesDelimiter : Regex.Regex
spacesDelimiter =
    Maybe.withDefault Regex.never <| Regex.fromString "\\s+"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTags ->
            let
                tags =
                    Regex.split spacesDelimiter (String.trim model.tagsInput)
            in
            ( { model | tags = tags, tagsInput = String.join " " tags }, updateTags tags )

        UpdateTagsInput tagsInput ->
            ( { model | tagsInput = tagsInput }, Cmd.none )

        ClipboardUpdated newEntry ->
            ( { model | kotuStatus = Loading }, kotuQuery newEntry )

        GotKotuResponse result ->
            case result of
                Ok sentences ->
                    let
                        components =
                            flatMap (\a -> a.components) <| flatMap (\a -> a.accentPhrases) sentences
                    in
                    ( { model | kotuStatus = Success components }, Cmd.none )

                Err _ ->
                    ( { model | kotuStatus = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    clipboard ClipboardUpdated



-- VIEW


view : Model -> Html Msg
view model =
    span []
        [ text "Tags:"
        , input
            [ style "margin-left" "red"
            , type_ "text"
            , value model.tagsInput
            , onInput UpdateTagsInput
            , onBlur UpdateTags
            ]
            []
        , text <|
            case model.kotuStatus of
                Success components ->
                    String.join "" <| List.map (\a -> a.surface) components

                Loading ->
                    "..."

                Failure ->
                    "Something went wrong."
        ]



-- HTTP


kotuQuery : String -> Cmd Msg
kotuQuery query =
    Http.post
        { url = "https://kotu.io/api/dictionary/parse"
        , body = Http.stringBody "text/plain" query
        , expect = Http.expectJson GotKotuResponse kotuDecoder
        }



--　猫が鳴いている。


kotuDecoder : Decoder (List KotuSentence)
kotuDecoder =
    list <|
        map KotuSentence
            (field "accentPhrases"
                (list <|
                    map KotuAccentPhrase
                        (field "components"
                            (list <|
                                map7 KotuComponent
                                    (field "surface" string)
                                    (field "kana" string)
                                    (field "surfaceOriginal" string)
                                    (field "originalKana" string)
                                    (field "pitchAccents" <|
                                        list <|
                                            map2 KotuPitchAccent
                                                (field "descriptive" string)
                                                (field "mora" int)
                                    )
                                    (field "isBasic" bool)
                                    (field "partOfSpeech" string)
                            )
                        )
                )
            )
