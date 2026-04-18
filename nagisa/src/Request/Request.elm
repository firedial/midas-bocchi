module Request.Request exposing
    ( Error(..)
    , deleteBalance
    , deleteFixedBalance
    , deleteMove
    , deleteTemplate
    , getAttributeCategories
    , getAttributeElement
    , getAttributeElements
    , getBalance
    , getBalances
    , getFixedBalance
    , getFixedBalances
    , getMove
    , getMoves
    , getTemplate
    , getTemplates
    , postAttributeElement
    , postBalance
    , postBonus
    , postCheckPlaceSum
    , postFixedBalance
    , postMove
    , postSalary
    , postTemplate
    , putAttributeElement
    , putBalance
    , putFixedBalance
    , putMove
    , putTemplate
    )

import Json.Decode as D
import Json.Encode as E
import Model.Enitity.AttributeCategoryEntity as AttributeCategoryEntity
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.BalanceEntity as BalanceEntity
import Model.Enitity.FixedBalanceEntity as FixedBalanceEntity
import Model.Enitity.MoveEntity as MoveEntity
import Model.Enitity.TemplateEntity as TemplateEntity
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
                |> required "group_id" D.int
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string
    in
    BaseRequest.get ("/api/balances/" ++ String.fromInt id) decodeBalance (toMsg << Result.mapError mapError)


getBalances : Int -> (Result Error BalanceEntity.Balances -> msg) -> Cmd msg
getBalances limit toMsg =
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
                |> required "group_id" D.int
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string

        decodeBalances =
            D.list decodeBalance
    in
    BaseRequest.get ("/api/balances?limit=" ++ String.fromInt limit ++ "&orderby=desc") decodeBalances (toMsg << Result.mapError mapError)


postBalance : BalanceEntity.NewBalance -> (Result Error () -> msg) -> Cmd msg
postBalance newBalance toMsg =
    let
        encodedNewBalance =
            E.object
                [ ( "amount", E.int newBalance.amount )
                , ( "item", E.string newBalance.item )
                , ( "kind_element_id", E.int newBalance.kindElementId )
                , ( "purpose_element_id", E.int newBalance.purposeElementId )
                , ( "place_element_id", E.int newBalance.placeElementId )
                , ( "date", E.string newBalance.date )
                , ( "group_id", newBalance.groupId |> Maybe.map E.int |> Maybe.withDefault E.null )
                ]
    in
    BaseRequest.post "/api/balances" encodedNewBalance (D.succeed ()) (toMsg << Result.mapError mapError)


putBalance : Int -> BalanceEntity.NewBalance -> (Result Error () -> msg) -> Cmd msg
putBalance id balance toMsg =
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
                , ( "group_id", balance.groupId |> Maybe.map E.int |> Maybe.withDefault E.null )
                ]
    in
    BaseRequest.put ("/api/balances/" ++ String.fromInt id) encodedBalance (D.succeed ()) (toMsg << Result.mapError mapError)


deleteBalance : Int -> (Result Error () -> msg) -> Cmd msg
deleteBalance balanceId toMsg =
    BaseRequest.delete ("/api/balances/" ++ String.fromInt balanceId) (D.succeed ()) (toMsg << Result.mapError mapError)


getFixedBalance : Int -> (Result Error FixedBalanceEntity.FixedBalance -> msg) -> Cmd msg
getFixedBalance id toMsg =
    let
        decodeFixedBalance =
            D.succeed FixedBalanceEntity.FixedBalance
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "kind_element_id" D.int
                |> required "purpose_element_id" D.int
                |> required "place_element_id" D.int
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string
    in
    BaseRequest.get ("/api/fixed_balances/" ++ String.fromInt id) decodeFixedBalance (toMsg << Result.mapError mapError)


getFixedBalances : (Result Error FixedBalanceEntity.FixedBalances -> msg) -> Cmd msg
getFixedBalances toMsg =
    let
        decodeFixedBalance =
            D.succeed FixedBalanceEntity.FixedBalance
                |> required "id" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "kind_element_id" D.int
                |> required "purpose_element_id" D.int
                |> required "place_element_id" D.int
                |> required "kind_element_description" D.string
                |> required "purpose_element_description" D.string
                |> required "place_element_description" D.string

        decodeFixedBalances =
            D.list decodeFixedBalance
    in
    BaseRequest.get "/api/fixed_balances" decodeFixedBalances (toMsg << Result.mapError mapError)


postFixedBalance : FixedBalanceEntity.NewFixedBalance -> (Result Error () -> msg) -> Cmd msg
postFixedBalance newFixedBalance toMsg =
    let
        encodedNewFixedBalance =
            E.object
                [ ( "amount", E.int newFixedBalance.amount )
                , ( "item", E.string newFixedBalance.item )
                , ( "kind_element_id", E.int newFixedBalance.kindElementId )
                , ( "purpose_element_id", E.int newFixedBalance.purposeElementId )
                , ( "place_element_id", E.int newFixedBalance.placeElementId )
                ]
    in
    BaseRequest.post "/api/fixed_balances" encodedNewFixedBalance (D.succeed ()) (toMsg << Result.mapError mapError)


putFixedBalance : Int -> FixedBalanceEntity.NewFixedBalance -> (Result Error () -> msg) -> Cmd msg
putFixedBalance id fixedBalance toMsg =
    let
        encodedFixedBalance =
            E.object
                [ ( "id", E.int id )
                , ( "amount", E.int fixedBalance.amount )
                , ( "item", E.string fixedBalance.item )
                , ( "kind_element_id", E.int fixedBalance.kindElementId )
                , ( "purpose_element_id", E.int fixedBalance.purposeElementId )
                , ( "place_element_id", E.int fixedBalance.placeElementId )
                ]
    in
    BaseRequest.put ("/api/fixed_balances/" ++ String.fromInt id) encodedFixedBalance (D.succeed ()) (toMsg << Result.mapError mapError)


deleteFixedBalance : Int -> (Result Error () -> msg) -> Cmd msg
deleteFixedBalance fixedBalanceId toMsg =
    BaseRequest.delete ("/api/fixed_balances/" ++ String.fromInt fixedBalanceId) (D.succeed ()) (toMsg << Result.mapError mapError)


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
                |> required "group_id" D.int
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
                |> required "group_id" D.int
                |> required "before_description" D.string
                |> required "after_description" D.string
    in
    BaseRequest.get ("/api/moves/" ++ mapMoveAttributeName moveAttributeValueObject ++ "s") (D.list decodeMove) (toMsg << Result.mapError mapError)


postMove : MoveAttributeValueObject.Attribute -> MoveEntity.NewMove -> (Result Error () -> msg) -> Cmd msg
postMove moveAttributeName newMove toMsg =
    let
        encodedNewMove =
            E.object
                [ ( "amount", E.int newMove.amount )
                , ( "item", E.string newMove.item )
                , ( "before_id", E.int newMove.beforeId )
                , ( "after_id", E.int newMove.afterId )
                , ( "date", E.string newMove.date )
                , ( "group_id", newMove.groupId |> Maybe.map E.int |> Maybe.withDefault E.null )
                ]
    in
    BaseRequest.post ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s") encodedNewMove (D.succeed ()) (toMsg << Result.mapError mapError)


putMove : MoveAttributeValueObject.Attribute -> Int -> MoveEntity.NewMove -> (Result Error () -> msg) -> Cmd msg
putMove moveAttributeName id move toMsg =
    let
        encodedMove =
            E.object
                [ ( "id", E.int id )
                , ( "amount", E.int move.amount )
                , ( "item", E.string move.item )
                , ( "before_id", E.int move.beforeId )
                , ( "after_id", E.int move.afterId )
                , ( "date", E.string move.date )
                , ( "group_id", move.groupId |> Maybe.map E.int |> Maybe.withDefault E.null )
                ]
    in
    BaseRequest.put ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s/" ++ String.fromInt id) encodedMove (D.succeed ()) (toMsg << Result.mapError mapError)


deleteMove : MoveAttributeValueObject.Attribute -> Int -> (Result Error () -> msg) -> Cmd msg
deleteMove moveAttributeName moveId toMsg =
    BaseRequest.delete ("/api/moves/" ++ mapMoveAttributeName moveAttributeName ++ "s/" ++ String.fromInt moveId) (D.succeed ()) (toMsg << Result.mapError mapError)


getTemplates : (Result Error TemplateEntity.Templates -> msg) -> Cmd msg
getTemplates toMsg =
    let
        decodeTemplate =
            D.succeed TemplateEntity.Template
                |> required "id" D.int
                |> required "name" D.string
    in
    BaseRequest.get "/api/templates" (D.list decodeTemplate) (toMsg << Result.mapError mapError)


getTemplate : Int -> (Result Error ( TemplateEntity.Template, List TemplateEntity.TemplateDetail ) -> msg) -> Cmd msg
getTemplate id toMsg =
    let
        decodeDetail =
            D.succeed TemplateEntity.TemplateDetail
                |> required "seq" D.int
                |> required "type" D.int
                |> required "amount" D.int
                |> required "item" D.string
                |> required "kind_element_id" D.int
                |> required "purpose_element_id" (D.nullable D.int)
                |> required "place_element_id" (D.nullable D.int)
                |> required "move_before_purpose_id" (D.nullable D.int)
                |> required "move_after_purpose_id" (D.nullable D.int)
                |> required "move_before_place_id" (D.nullable D.int)
                |> required "move_after_place_id" (D.nullable D.int)

        decodeResponse =
            D.map2
                (\template details -> ( template, details ))
                (D.succeed TemplateEntity.Template
                    |> required "id" D.int
                    |> required "name" D.string
                )
                (D.field "details" (D.list decodeDetail))
    in
    BaseRequest.get ("/api/templates/" ++ String.fromInt id) decodeResponse (toMsg << Result.mapError mapError)


postTemplate : TemplateEntity.NewTemplate -> (Result Error () -> msg) -> Cmd msg
postTemplate newTemplate toMsg =
    let
        encodeDetail d =
            E.object
                ([ ( "type", E.int d.type_ )
                 , ( "amount", E.int d.amount )
                 , ( "item", E.string d.item )
                 , ( "kind_element_id", E.int d.kindElementId )
                 , ( "purpose_element_id", encodeMaybeInt d.purposeElementId )
                 , ( "place_element_id", encodeMaybeInt d.placeElementId )
                 , ( "move_before_purpose_id", encodeMaybeInt d.moveBeforePurposeId )
                 , ( "move_after_purpose_id", encodeMaybeInt d.moveAfterPurposeId )
                 , ( "move_before_place_id", encodeMaybeInt d.moveBeforePlaceId )
                 , ( "move_after_place_id", encodeMaybeInt d.moveAfterPlaceId )
                 ]
                )

        encoded =
            E.object
                [ ( "name", E.string newTemplate.name )
                , ( "details", E.list encodeDetail newTemplate.details )
                ]
    in
    BaseRequest.post "/api/templates" encoded (D.succeed ()) (toMsg << Result.mapError mapError)


putTemplate : Int -> TemplateEntity.NewTemplate -> (Result Error () -> msg) -> Cmd msg
putTemplate id newTemplate toMsg =
    let
        encodeDetail d =
            E.object
                [ ( "type", E.int d.type_ )
                , ( "amount", E.int d.amount )
                , ( "item", E.string d.item )
                , ( "kind_element_id", E.int d.kindElementId )
                , ( "purpose_element_id", encodeMaybeInt d.purposeElementId )
                , ( "place_element_id", encodeMaybeInt d.placeElementId )
                , ( "move_before_purpose_id", encodeMaybeInt d.moveBeforePurposeId )
                , ( "move_after_purpose_id", encodeMaybeInt d.moveAfterPurposeId )
                , ( "move_before_place_id", encodeMaybeInt d.moveBeforePlaceId )
                , ( "move_after_place_id", encodeMaybeInt d.moveAfterPlaceId )
                ]

        encoded =
            E.object
                [ ( "name", E.string newTemplate.name )
                , ( "details", E.list encodeDetail newTemplate.details )
                ]
    in
    BaseRequest.put ("/api/templates/" ++ String.fromInt id) encoded (D.succeed ()) (toMsg << Result.mapError mapError)


deleteTemplate : Int -> (Result Error () -> msg) -> Cmd msg
deleteTemplate templateId toMsg =
    BaseRequest.delete ("/api/templates/" ++ String.fromInt templateId) (D.succeed ()) (toMsg << Result.mapError mapError)


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


postAttributeElement : AttributeValueObject.Attribute -> AttributeElementEntity.NewAttributeElement -> (Result Error () -> msg) -> Cmd msg
postAttributeElement attributeValueObject newAttributeElement toMsg =
    let
        encodedNewAttributeElement =
            E.object
                [ ( "name", E.string newAttributeElement.name )
                , ( "description", E.string newAttributeElement.description )
                , ( "priority", E.int newAttributeElement.priority )
                , ( "category_id", E.int newAttributeElement.categoryId )
                ]
    in
    BaseRequest.post ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element") encodedNewAttributeElement (D.succeed ()) (toMsg << Result.mapError mapError)


putAttributeElement : AttributeValueObject.Attribute -> Int -> AttributeElementEntity.NewAttributeElement -> (Result Error () -> msg) -> Cmd msg
putAttributeElement attributeValueObject id attributeElement toMsg =
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
    BaseRequest.put ("/api/attribute_elements/" ++ mapAttributeName attributeValueObject ++ "_element/" ++ String.fromInt id) encodedAttributeElement (D.succeed ()) (toMsg << Result.mapError mapError)


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


postSalary : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> (Result Error () -> msg) -> Cmd msg
postSalary baseSalary adjustmentSalary transportation holdingIncentives healthInsurance welfarePension residentTax employmentInsurance incomeTax holding date toMsg =
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
    BaseRequest.post "/api/salary" encodedSalary (D.succeed ()) (toMsg << Result.mapError mapError)


postBonus : Int -> Int -> Int -> Int -> Int -> String -> (Result Error () -> msg) -> Cmd msg
postBonus bonus healthInsurance welfarePension employmentInsurance incomeTax date toMsg =
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
    BaseRequest.post "/api/bonus" encodedBonus (D.succeed ()) (toMsg << Result.mapError mapError)


postCheckPlaceSum : Int -> Int -> String -> (Result Error () -> msg) -> Cmd msg
postCheckPlaceSum sum placeElementId date toMsg =
    let
        encodedCheckPlaceSum =
            E.object
                [ ( "sum", E.string <| String.fromInt sum )
                , ( "placeElementId", E.string <| String.fromInt placeElementId )
                , ( "date", E.string date )
                ]
    in
    BaseRequest.post "/api/check_place_sum" encodedCheckPlaceSum (D.succeed ()) (toMsg << Result.mapError mapError)


encodeMaybeInt : Maybe Int -> E.Value
encodeMaybeInt maybeInt =
    case maybeInt of
        Nothing ->
            E.null

        Just i ->
            E.int i


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
