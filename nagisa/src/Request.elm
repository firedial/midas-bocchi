module Request exposing (deleteBalance, getBalances, postBalance, postLogin)

import BaseRequest
import Enitity.BalanceEntity as BalanceEntity
import Json.Decode as Decode
import Json.Encode as Encode


getBalances : (Result String BalanceEntity.Balances -> msg) -> Cmd msg
getBalances toMsg =
    BaseRequest.get "api/balances" BalanceEntity.decodeBalances toMsg


postBalance : String -> BalanceEntity.NewBalance -> (Result String () -> msg) -> Cmd msg
postBalance xsrfToken newBalance toMsg =
    BaseRequest.post xsrfToken "api/balances" (BalanceEntity.encodeNewBalance newBalance) (Decode.succeed ()) toMsg


deleteBalance : String -> Int -> (Result String () -> msg) -> Cmd msg
deleteBalance xsrfToken balanceId toMsg =
    BaseRequest.delete xsrfToken ("api/balances/" ++ String.fromInt balanceId) (Decode.succeed ()) toMsg


postLogin : String -> String -> String -> (Result String () -> msg) -> Cmd msg
postLogin xsrfToken email password toMsg =
    let
        data =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]
    in
    BaseRequest.post xsrfToken "api/login" data (Decode.succeed ()) toMsg
