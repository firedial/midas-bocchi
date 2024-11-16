module Model.Enitity.BalanceEntity exposing (Balance, Balances, NewBalance)


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
