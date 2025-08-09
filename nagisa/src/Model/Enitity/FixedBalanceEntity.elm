module Model.Enitity.FixedBalanceEntity exposing (FixedBalance, FixedBalances, NewFixedBalance)


type alias FixedBalance =
    { fixedBalanceId : Int
    , amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Int
    , placeElementId : Int
    , kindElementDescription : String
    , purposeElementDescription : String
    , placeElementDescription : String
    }


type alias FixedBalances =
    List FixedBalance


type alias NewFixedBalance =
    { amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Int
    , placeElementId : Int
    }
