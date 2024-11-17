module Model.Enitity.AttributeCategoryEntity exposing (AttributeCategories, AttributeCategory)


type alias AttributeCategory =
    { id : Int
    , name : String
    , desription : String
    }


type alias AttributeCategories =
    List AttributeCategory
