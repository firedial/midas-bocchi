module Request.Request exposing
    ( Error(..)
    , deleteBalance
    , deleteMove
    , getAttributeCategories
    , getAttributeElement
    , getAttributeElements
    , getBalance
    , getBalances
    , getMove
    , getMoves
    , postAttributeElement
    , postBalance
    , postBonus
    , postLogin
    , postLogout
    , postMove
    , postSalary
    , putAttributeElement
    , putBalance
    , putMove
    )

import Json.Decode as D
import Json.Encode as E
import Model.Enitity.AttributeCategoryEntity as AttributeCategoryEntity
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.BalanceEntity as BalanceEntity
import Model.Enitity.MoveEntity as MoveEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Request.BaseRequest as BaseRequest
import Result


type Error
    = DecodeError String
    | RequestError String


getBalance : Int -> (Result Error BalanceEntity.Balance -> msg) -> Cmd msg
getBalance id toMsg =
    let
        decodeBalance =
            D.succeed BalanceEntity.Balance
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "kind_element_id" D.int
                |> required "purpose_element_id" D.int
                |> required "place_element_id" D.int
                |> required "date" D.string
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string
    in
    BaseRequest.get ("/api/balances/" ++ String.fromInt id) decodeBalance (toMsg << Result.mapError mapError)


getBalances : (Result Error BalanceEntity.Balances -> msg) -> Cmd msg
getBalances toMsg =
    let
        decodeBalance =
            D.succeed BalanceEntity.Balance
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "kind_element_id" D.int
                |> required "purpose_element_id" D.int
                |> required "place_element_id" D.int
                |> required "date" D.string
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string

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


putBalance : String -> Int -> BalanceEntity.NewBalance -> (Result Error () -> msg) -> Cmd msg
putBalance xsrfToken id balance toMsg =
    let
        encodedBalance =
            E.object
                [ ( "id", E.int id )
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


getMove : MoveAttributeValueObject.Attribute -> Int -> (Result Error MoveEntity.Move -> msg) -> Cmd msg
getMove moveAttributeValueObject id toMsg =
    let
        decodeMove =
            D.succeed MoveEntity.Move
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "before_id" D.int
                |> required "after_id" D.int
                |> required "date" D.string
                |> required "before_description" D.string
                |> required "after_description" D.string
    in
    BaseRequest.get ("/api/moves/" ++ mapMoveAttributeName moveAttributeValueObject ++ "s/" ++ String.fromInt id) decodeMove (toMsg << Result.mapError mapError)


getMoves : MoveAttributeValueObject.Attribute -> (Result Error MoveEntity.Moves -> msg) -> Cmd msg
getMoves moveAttributeValueObject toMsg =
    let
        decodeMove =
            D.succeed MoveEntity.Move
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "before_id" D.int
                |> required "after_id" D.int
                |> required "date" D.string
                |> required "before_description" D.string
                |> required "after_description" D.string
    in
    BaseRequest.get ("/api/moves/" ++ mapMoveAttributeName moveAttributeValueObject ++ "s") (D.list decodeMove) (toMsg << Result.mapError mapError)


postMove : String -> MoveAttributeValueObject.Attribute -> MoveEntity.NewMove -> (Result Error () -> msg) -> Cmd msg
postMove xsrfToken moveAttributeName newMove toMsg =
    let
        encodedNewMove =
            E.object
                [ ( "amount", E.int newMove.amount )
                , ( "item", E.string newMove.item )
                , ( "before_id", E.int newMove.beforeId )
                , ( "after_id", E.int newMove.afterId )
                , ( "date", E.string newMove.date )
                ]
    in
    BaseRequest.post xsrfToken ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s") encodedNewMove (D.succeed ()) (toMsg << Result.mapError mapError)


putMove : String -> MoveAttributeValueObject.Attribute -> Int -> MoveEntity.NewMove -> (Result Error () -> msg) -> Cmd msg
putMove xsrfToken moveAttributeName id move toMsg =
    let
        encodedMove =
            E.object
                [ ( "id", E.int id )
                , ( "amount", E.int move.amount )
                , ( "item", E.string move.item )
                , ( "before_id", E.int move.beforeId )
                , ( "after_id", E.int move.afterId )
                , ( "date", E.string move.date )
                ]
    in
    BaseRequest.put xsrfToken ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s/" ++ String.fromInt id) encodedMove (D.succeed ()) (toMsg << Result.mapError mapError)


deleteMove : String -> MoveAttributeValueObject.Attribute -> Int -> (Result Error () -> msg) -> Cmd msg
deleteMove xsrfToken moveAttributeName moveId toMsg =
    BaseRequest.delete xsrfToken ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s/" ++ String.fromInt moveId) (D.succeed ()) (toMsg << Result.mapError mapError)


getAttributeElement : AttributeValueObject.Attribute -> Int -> (Result Error AttributeElementEntity.AttributeElement -> msg) -> Cmd msg
getAttributeElement attributeValueObject id toMsg =
    let
        decodeAttributeElement =
            D.succeed AttributeElementEntity.AttributeElement
                |> required "id" D.int
                |> required "name" D.string
                |> required "description" D.string
                |> required "priority" D.int
                |> required "category_id" D.int
    in
    BaseRequest.get ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element/" ++ String.fromInt id) decodeAttributeElement (toMsg << Result.mapError mapError)


getAttributeElements : AttributeValueObject.Attribute -> (Result Error AttributeElementEntity.AttributeElements -> msg) -> Cmd msg
getAttributeElements attributeValueObject toMsg =
    let
        decodeAttributeElement =
            D.succeed AttributeElementEntity.AttributeElement
                |> required "id" D.int
                |> required "name" D.string
                |> required "description" D.string
                |> required "priority" D.int
                |> required "category_id" D.int
    in
    BaseRequest.get ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element") (D.list decodeAttributeElement) (toMsg << Result.mapError mapError)


postAttributeElement : String -> AttributeValueObject.Attribute -> AttributeElementEntity.NewAttributeElement -> (Result Error () -> msg) -> Cmd msg
postAttributeElement xsrfToken attributeValueObject newAttributeElement toMsg =
    let
        encodedNewAttributeElement =
            E.object
                [ ( "name", E.string newAttributeElement.name )
                , ( "description", E.string newAttributeElement.description )
                , ( "priority", E.int newAttributeElement.priority )
                , ( "category_id", E.int newAttributeElement.categoryId )
                ]
    in
    BaseRequest.post xsrfToken ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element") encodedNewAttributeElement (D.succeed ()) (toMsg << Result.mapError mapError)


putAttributeElement : String -> AttributeValueObject.Attribute -> Int -> AttributeElementEntity.NewAttributeElement -> (Result Error () -> msg) -> Cmd msg
putAttributeElement xsrfToken attributeValueObject id attributeElement toMsg =
    let
        encodedAttributeElement =
            E.object
                [ ( "id", E.int id )
                , ( "name", E.string attributeElement.name )
                , ( "description", E.string attributeElement.description )
                , ( "priority", E.int attributeElement.priority )
                , ( "category_id", E.int attributeElement.categoryId )
                ]
    in
    BaseRequest.put xsrfToken ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element/" ++ String.fromInt id) encodedAttributeElement (D.succeed ()) (toMsg << Result.mapError mapError)


getAttributeCategories : AttributeValueObject.Attribute -> (Result Error AttributeCategoryEntity.AttributeCategories -> msg) -> Cmd msg
getAttributeCategories attributeValueObject toMsg =
    let
        decodeAttributeCategory =
            D.succeed AttributeCategoryEntity.AttributeCategory
                |> required "id" D.int
                |> required "name" D.string
                |> required "description" D.string
    in
    BaseRequest.get ("/api/attribute_categories/" ++ mapAttributeName attributeValueObject ++ "_category") (D.list decodeAttributeCategory) (toMsg << Result.mapError mapError)


postSalary : String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> (Result Error () -> msg) -> Cmd msg
postSalary xsrfToken baseSalary adjustmentSalary transportation holdingIncentives healthInsurance welfarePension residentTax employmentInsurance incomeTax holding date toMsg =
    let
        encodedSalary =
            E.object
                [ ( "baseSalary", E.int baseSalary )
                , ( "adjustmentSalary", E.int adjustmentSalary )
                , ( "transportation", E.int transportation )
                , ( "holdingIncentives", E.int holdingIncentives )
                , ( "healthInsurance", E.int healthInsurance )
                , ( "welfarePension", E.int welfarePension )
                , ( "residentTax", E.int residentTax )
                , ( "employmentInsurance", E.int employmentInsurance )
                , ( "incomeTax", E.int incomeTax )
                , ( "holding", E.int holding )
                , ( "date", E.string date )
                ]
    in
    BaseRequest.post xsrfToken "/api/salary" encodedSalary (D.succeed ()) (toMsg << Result.mapError mapError)


postBonus : String -> Int -> Int -> Int -> Int -> Int -> String -> (Result Error () -> msg) -> Cmd msg
postBonus xsrfToken bonus healthInsurance welfarePension employmentInsurance incomeTax date toMsg =
    let
        encodedBonus =
            E.object
                [ ( "bonus", E.int bonus )
                , ( "healthInsurance", E.int healthInsurance )
                , ( "welfarePension", E.int welfarePension )
                , ( "employmentInsurance", E.int employmentInsurance )
                , ( "incomeTax", E.int incomeTax )
                , ( "date", E.string date )
                ]
    in
    BaseRequest.post xsrfToken "/api/bonus" encodedBonus (D.succeed ()) (toMsg << Result.mapError mapError)


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


postLogout : String -> (Result Error () -> msg) -> Cmd msg
postLogout xsrfToken toMsg =
    BaseRequest.post xsrfToken "/api/logout" (E.object []) (D.succeed ()) (toMsg << Result.mapError mapError)


mapAttributeName : AttributeValueObject.Attribute -> String
mapAttributeName attributeName =
    case attributeName of
        AttributeValueObject.Kind ->
            "kind"

        AttributeValueObject.Purpose ->
            "purpose"

        AttributeValueObject.Place ->
            "place"


mapMoveAttributeName : MoveAttributeValueObject.Attribute -> String
mapMoveAttributeName moveAttributeName =
    case moveAttributeName of
        MoveAttributeValueObject.Purpose ->
            "purpose"

        MoveAttributeValueObject.Place ->
            "place"


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


required : String -> D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
required key decoder pipeDecoder =
    D.map2 (|>) (D.field key decoder) pipeDecoder
