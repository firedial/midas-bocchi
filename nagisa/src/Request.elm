module Request exposing (getBalances)

import BaseRequest
import Enitity.BalanceEntity as BalanceEntity


getBalances : (Result String BalanceEntity.Balances -> msg) -> Cmd msg
getBalances toMsg =
    BaseRequest.get "api/balances" BalanceEntity.decodeBalances toMsg
