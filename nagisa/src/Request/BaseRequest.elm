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


get : String -> String -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
get apiKey url decoder toMsg =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ apiKey)
            ]
        , url = url
        , body = Http.emptyBody
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> String -> Json.Encode.Value -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
post apiKey url body decoder toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ apiKey)
            ]
        , url = url
        , body = Http.jsonBody body
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



put : String -> String -> Json.Encode.Value -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
put apiKey url body decoder toMsg =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ apiKey)
            ]
        , url = url
        , body = Http.jsonBody body
        , expect = expect decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> String -> Json.Decode.Decoder a -> (Result Error a -> msg) -> Cmd msg
delete apiKey url decoder toMsg =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ apiKey)
            ]
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
                            -- JSONデコードに失敗した場合は生のボディを返す
                            Err (BadStatusDecodeError body)

                Http.GoodStatus_ _ body ->
                    -- 正常なレスポンスの場合はデコード
                    let
                        -- 空文字列を JSON に変換できないのでこうしている
                        -- @todo それを直す
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
