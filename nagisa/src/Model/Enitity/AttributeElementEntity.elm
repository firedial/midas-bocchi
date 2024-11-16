module Model.Enitity.AttributeElementEntity exposing (AttributeElement, AttributeElements)


type alias AttributeElement =
    { id : Int
    , name : String
    , desription : String
    , priority : Int
    , categoryId : Int
    }


type alias AttributeElements =
    List AttributeElement
