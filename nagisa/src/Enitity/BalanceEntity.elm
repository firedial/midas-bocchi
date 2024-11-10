module Enitity.BalanceEntity exposing (Balance, Balances, NewBalance, decodeBalance, decodeBalances, encodeNewBalance)

import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E


type alias Balance =
    { balanceId : Int
    , amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Int
    , placeElementId : Int
    , date : String
    , kindElementDescription : String
    , purposeElementDescription : String
    , placeElementDescription : String
    }


type alias Balances =
    List Balance


type alias NewBalance =
    { amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Int
    , placeElementId : Int
    , date : String
    }


decodeBalances : D.Decoder Balances
decodeBalances =
    D.list decodeBalance


decodeBalance : D.Decoder Balance
decodeBalance =
    D.succeed Balance
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


encodeNewBalance : NewBalance -> E.Value
encodeNewBalance balance =
    E.object
        [ ( "amount", E.int balance.amount )
        , ( "item", E.string balance.item )
        , ( "kind_element_id", E.int balance.kindElementId )
        , ( "purpose_element_id", E.int balance.purposeElementId )
        , ( "place_element_id", E.int balance.placeElementId )
        , ( "date", E.string balance.date )
        ]
