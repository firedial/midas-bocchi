module Request.Request exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.BalanceEntity as BalanceEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.BaseRequest as BaseRequest
import Result


type Error
    = DecodeError String
    | RequestError String


getBalances : (Result Error BalanceEntity.Balances -> msg) -> Cmd msg
getBalances toMsg =
    let
        decodeBalance =
            D.succeed BalanceEntity.Balance
                |> DP.required "id" D.int
                |> DP.required "amount" D.int
                |> DP.required "item" D.string
                |> DP.required "kind_element_id" D.int
                |> DP.required "purpose_element_id" D.int
                |> DP.required "place_element_id" D.int
                |> DP.required "date" D.string
                |> DP.required "kind_element_description" D.string
                |> DP.required "purpose_element_description" D.string
                |> DP.required "place_element_description" D.string

        decodeBalances =
            D.list decodeBalance
    in
    BaseRequest.get "/api/balances" decodeBalances (toMsg << Result.mapError mapError)


postBalance : String -> BalanceEntity.NewBalance -> (Result Error () -> msg) -> Cmd msg
postBalance xsrfToken newBalance toMsg =
    let
        encodedNewBalance =
            E.object
                [ ( "amount", E.int newBalance.amount )
                , ( "item", E.string newBalance.item )
                , ( "kind_element_id", E.int newBalance.kindElementId )
                , ( "purpose_element_id", E.int newBalance.purposeElementId )
                , ( "place_element_id", E.int newBalance.placeElementId )
                , ( "date", E.string newBalance.date )
                ]
    in
    BaseRequest.post xsrfToken "/api/balances" encodedNewBalance (D.succeed ()) (toMsg << Result.mapError mapError)


putBalance : String -> BalanceEntity.Balance -> (Result Error () -> msg) -> Cmd msg
putBalance xsrfToken balance toMsg =
    let
        encodedBalance =
            E.object
                [ ( "id", E.int balance.balanceId )
                , ( "amount", E.int balance.amount )
                , ( "item", E.string balance.item )
                , ( "kind_element_id", E.int balance.kindElementId )
                , ( "purpose_element_id", E.int balance.purposeElementId )
                , ( "place_element_id", E.int balance.placeElementId )
                , ( "date", E.string balance.date )
                ]
    in
    BaseRequest.put xsrfToken "/api/balances/1/" encodedBalance (D.succeed ()) (toMsg << Result.mapError mapError)


deleteBalance : String -> Int -> (Result Error () -> msg) -> Cmd msg
deleteBalance xsrfToken balanceId toMsg =
    BaseRequest.delete xsrfToken ("/api/balances/" ++ String.fromInt balanceId) (D.succeed ()) (toMsg << Result.mapError mapError)


getAttributeElements : AttributeValueObject.Attribute -> (Result Error AttributeElementEntity.AttributeElements -> msg) -> Cmd msg
getAttributeElements attributeValueObject toMsg =
    let
        decodeAttributeElement =
            D.succeed AttributeElementEntity.AttributeElement
                |> DP.required "id" D.int
                |> DP.required "name" D.string
                |> DP.required "description" D.string
                |> DP.required "priority" D.int
                |> DP.required "category_id" D.int

        attributeName =
            case attributeValueObject of
                AttributeValueObject.Kind ->
                    "kind"

                AttributeValueObject.Purpose ->
                    "purpose"

                AttributeValueObject.Place ->
                    "place"
    in
    BaseRequest.get ("/api/attribute_elements/" ++ attributeName ++ "_element") (D.list decodeAttributeElement) (toMsg << Result.mapError mapError)


postLogin : String -> String -> String -> (Result Error () -> msg) -> Cmd msg
postLogin xsrfToken email password toMsg =
    let
        data =
            E.object
                [ ( "email", E.string email )
                , ( "password", E.string password )
                ]
    in
    BaseRequest.post xsrfToken "/api/login" data (D.succeed ()) (toMsg << Result.mapError mapError)


mapError : BaseRequest.Error -> Error
mapError baseRequestError =
    case baseRequestError of
        BaseRequest.BadUrl url ->
            RequestError ("Bad Url: " ++ url)

        BaseRequest.Timeout ->
            RequestError "Timeout"

        BaseRequest.NetworkError ->
            RequestError "NetworkError"

        BaseRequest.BadStatus body ->
            RequestError ("BadStatus: " ++ body)

        BaseRequest.BadStatusDecodeError body ->
            RequestError ("BadStatusDecodeError: " ++ body)

        BaseRequest.GoodStatusDecodeError body ->
            DecodeError ("GoodStatusDecodeError: " ++ body)
