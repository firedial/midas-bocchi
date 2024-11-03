module Request exposing (getBalances)

import Enitity.BalanceEntity as BalanceEntity
import Http
import Result


getBalances : (Result String BalanceEntity.Balances -> msg) -> Cmd msg
getBalances result =
    Http.get { url = "/api/balances", expect = Http.expectJson (Result.mapError httpErrorToString >> result) BalanceEntity.decodeBalances }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error occurred"

        Http.BadStatus statusCode ->
            "Request failed with status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Failed to decode response: " ++ message
