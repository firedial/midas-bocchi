module Model.Enitity.AttributeCategoryEntity exposing (AttributeCategories, AttributeCategory)


type alias AttributeCategory =
    { id : Int
    , name : String
    , description : String
    }


type alias AttributeCategories =
    List AttributeCategory
