module Request.BaseRequest exposing (Error(..), delete, get, post, put)

import Http
import Json.Decode
import Json.Encode


type alias ErrorResponse =
    { message : String
    }


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus String
    | BadStatusDecodeError String
    | GoodStatusDecodeError String


get : String -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
get url decoder toMsg =
    Http.get { url = url, expect = expect decoder toMsg }


post : String -> Json.Encode.Value -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
post url body decoder toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


put : String -> Json.Encode.Value -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
put url body decoder toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
delete url decoder toMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


errorDecoder : Json.Decode.Decoder ErrorResponse
errorDecoder =
    Json.Decode.map ErrorResponse
        (Json.Decode.field "message" Json.Decode.string)


expect : Json.Decode.Decoder a -> (Result Error a -> msg) -> Http.Expect msg
expect decoder toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ _ body ->
                    case Json.Decode.decodeString errorDecoder body of
                        Ok errorResponse ->
                            Err (BadStatus errorResponse.message)

                        Err _ ->
                            Err (BadStatusDecodeError body)

                Http.GoodStatus_ _ body ->
                    let
                        body_ =
                            if body == "" then
                                "1"

                            else
                                body
                    in
                    case Json.Decode.decodeString decoder body_ of
                        Ok value ->
                            Ok value

                        Err error ->
                            Err (GoodStatusDecodeError (Json.Decode.errorToString error))
