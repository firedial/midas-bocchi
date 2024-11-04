module BaseRequest exposing (get)

import Http
import Json.Decode


type alias ErrorResponse =
    { message : String
    }


get : String -> Json.Decode.Decoder a -> (Result String a -> msg) -> Cmd msg
get url decoder toMsg =
    Http.get { url = url, expect = getExpect decoder toMsg }


errorDecoder : Json.Decode.Decoder ErrorResponse
errorDecoder =
    Json.Decode.map ErrorResponse
        (Json.Decode.field "message" Json.Decode.string)


getExpect : Json.Decode.Decoder a -> (Result String a -> msg) -> Http.Expect msg
getExpect decoder toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ("Invalid URL: " ++ url)

                Http.Timeout_ ->
                    Err "Request timed out"

                Http.NetworkError_ ->
                    Err "Network error occurred"

                Http.BadStatus_ _ body ->
                    case Json.Decode.decodeString errorDecoder body of
                        Ok errorResponse ->
                            Err ("Server error: " ++ errorResponse.message)

                        Err _ ->
                            -- JSONデコードに失敗した場合は生のボディを返す
                            Err ("Server error: " ++ body)

                Http.GoodStatus_ _ body ->
                    -- 正常なレスポンスの場合はデコード
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err error ->
                            Err ("Failed to decode: " ++ Json.Decode.errorToString error)
