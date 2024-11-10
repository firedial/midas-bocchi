module Request exposing (getBalances, postBalance)

import BaseRequest
import Enitity.BalanceEntity as BalanceEntity
import Json.Decode


getBalances : (Result String BalanceEntity.Balances -> msg) -> Cmd msg
getBalances toMsg =
    BaseRequest.get "api/balances" BalanceEntity.decodeBalances toMsg


postBalance : BalanceEntity.NewBalance -> (Result String String -> msg) -> Cmd msg
postBalance newBalance toMsg =
    BaseRequest.post "api/balances" (BalanceEntity.encodeNewBalance newBalance) decodeString toMsg


decodeString : Json.Decode.Decoder String
decodeString =
    Json.Decode.succeed "ok"
