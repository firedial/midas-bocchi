module Model.Enitity.AttributeElementEntity exposing (AttributeElement, AttributeElements, NewAttributeElement)


type alias AttributeElement =
    { id : Int
    , name : String
    , description : String
    , priority : Int
    , categoryId : Int
    }


type alias AttributeElements =
    List AttributeElement
type alias NewAttributeElement =
    { name : String
    , description : String
    , priority : Int
    , categoryId : Int
    }

