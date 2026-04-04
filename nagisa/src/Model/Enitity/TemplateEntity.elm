module Model.Enitity.TemplateEntity exposing (NewTemplate, NewTemplateDetail, Template, TemplateDetail, Templates)


type alias Template =
    { id : Int
    , name : String
    }


type alias Templates =
    List Template


type alias TemplateDetail =
    { seq : Int
    , type_ : Int
    , amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Maybe Int
    , placeElementId : Maybe Int
    , moveBeforePurposeId : Maybe Int
    , moveAfterPurposeId : Maybe Int
    , moveBeforePlaceId : Maybe Int
    , moveAfterPlaceId : Maybe Int
    }


type alias NewTemplate =
    { name : String
    , details : List NewTemplateDetail
    }


type alias NewTemplateDetail =
    { type_ : Int
    , amount : Int
    , item : String
    , kindElementId : Int
    , purposeElementId : Maybe Int
    , placeElementId : Maybe Int
    , moveBeforePurposeId : Maybe Int
    , moveAfterPurposeId : Maybe Int
    , moveBeforePlaceId : Maybe Int
    , moveAfterPlaceId : Maybe Int
    }
