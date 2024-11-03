module Enitity.BalanceEntity exposing (Balance, Balances, decodeBalance, decodeBalances)

import Json.Decode as D
import Json.Decode.Pipeline as DP


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

decodeBalances : D.Decoder Balances
decodeBalances = D.list decodeBalance

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



-- encodeBalance : Balance -> Encode.Value
-- encodeBalance balance =
--     Encode.object
--         [ ( "amount", Encode.int balance.amount )
--         , ( "item", Encode.string balance.item )
--         , ( "kind_id", Encode.int balance.kindId )
--         , ( "purpose_id", Encode.int balance.purposeId )
--         , ( "place_id", Encode.int balance.placeId )
--         , ( "date", Encode.string balance.date )
--         ]
