module Page.BalanceId exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onCheck, onClick, onInput)
import List
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.BalanceEntity as BalanceEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { balance : StringBalance
    , kindElements : AttributeElementEntity.AttributeElements
    , purposeElements : AttributeElementEntity.AttributeElements
    , placeElements : AttributeElementEntity.AttributeElements
    , xsrfToken : String
    , id : Maybe Int
    , isRemainAmount : Bool
    , isRemainItem : Bool
    , isRemainKind : Bool
    , isRemainPurpose : Bool
    , isRemainPlace : Bool
    , isRemainDate : Bool
    , enableInputDeleteString : Bool
    , deleteString : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringBalance =
    { amount : String
    , item : String
    , kindElementId : String
    , purposeElementId : String
    , placeElementId : String
    , date : String
    }


type Msg
    = InputAmount String
    | InputItem String
    | InputKindElementId String
    | InputPurposeElementId String
    | InputPlaceElementId String
    | InputDate String
    | GetBalance (Result Request.Error BalanceEntity.Balance)
    | GetAttributeElements AttributeValueObject.Attribute (Result Request.Error AttributeElementEntity.AttributeElements)
    | Upsert
    | Cancel
    | Delete Int
    | InputDeleteString String
    | ModifiedResult (Result Request.Error ())
    | CheckRemainAmount Bool
    | CheckRemainItem Bool
    | CheckRemainKind Bool
    | CheckRemainPurpose Bool
    | CheckRemainPlace Bool
    | CheckRemainDate Bool


init : String -> Navigation.Key -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key id =
    ( Model
        (StringBalance
            ""
            ""
            ""
            ""
            ""
            ""
        )
        []
        []
        []
        xsrfToken
        id
        False
        False
        False
        False
        False
        False
        False
        ""
        key
        Nothing
    , Cmd.batch
        ([ Request.getAttributeElements AttributeValueObject.Kind (GetAttributeElements AttributeValueObject.Kind)
         , Request.getAttributeElements AttributeValueObject.Purpose (GetAttributeElements AttributeValueObject.Purpose)
         , Request.getAttributeElements AttributeValueObject.Place (GetAttributeElements AttributeValueObject.Place)
         ]
            ++ (case id of
                    Nothing ->
                        []

                    Just id_ ->
                        [ Request.getBalance id_ GetBalance
                        ]
               )
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputAmount amount ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | amount = amount } }, Cmd.none )

        InputItem item ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | item = item } }, Cmd.none )

        InputKindElementId id ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | kindElementId = id } }, Cmd.none )

        InputPurposeElementId id ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | purposeElementId = id } }, Cmd.none )

        InputPlaceElementId id ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | placeElementId = id } }, Cmd.none )

        InputDate date ->
            let
                newBalance =
                    model.balance
            in
            ( { model | balance = { newBalance | date = date } }, Cmd.none )

        GetBalance result ->
            case result of
                Ok balance ->
                    let
                        stringBalance =
                            StringBalance
                                (String.fromInt balance.amount)
                                balance.item
                                (String.fromInt balance.kindElementId)
                                (String.fromInt balance.purposeElementId)
                                (String.fromInt balance.placeElementId)
                                balance.date
                    in
                    ( { model | balance = stringBalance }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        GetAttributeElements attributeName result ->
            case result of
                Ok attributeElements ->
                    let
                        newModel =
                            case attributeName of
                                AttributeValueObject.Kind ->
                                    { model | kindElements = attributeElements }

                                AttributeValueObject.Purpose ->
                                    { model | purposeElements = attributeElements }

                                AttributeValueObject.Place ->
                                    { model | placeElements = attributeElements }
                    in
                    ( newModel, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Upsert ->
            let
                newBalance =
                    BalanceEntity.NewBalance
                        (model.balance.amount |> String.toInt |> Maybe.withDefault 0)
                        model.balance.item
                        (model.balance.kindElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.balance.purposeElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.balance.placeElementId |> String.toInt |> Maybe.withDefault 0)
                        model.balance.date

                cmd =
                    case model.id of
                        Nothing ->
                            Request.postBalance model.xsrfToken newBalance ModifiedResult

                        Just id ->
                            Request.putBalance model.xsrfToken id newBalance ModifiedResult
            in
            ( model, cmd )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.BalanceTable) )

        Delete id ->
            if model.deleteString == "delete" then
                ( model, Request.deleteBalance model.xsrfToken id ModifiedResult )

            else
                ( { model | enableInputDeleteString = True }, Cmd.none )

        InputDeleteString deleteString ->
            ( { model | deleteString = deleteString }, Cmd.none )

        ModifiedResult result ->
            case result of
                Ok _ ->
                    case model.id of
                        Nothing ->
                            let
                                stringBalance =
                                    StringBalance
                                        (if model.isRemainAmount then
                                            model.balance.amount

                                         else
                                            ""
                                        )
                                        (if model.isRemainItem then
                                            model.balance.item

                                         else
                                            ""
                                        )
                                        (if model.isRemainKind then
                                            model.balance.kindElementId

                                         else
                                            ""
                                        )
                                        (if model.isRemainPurpose then
                                            model.balance.purposeElementId

                                         else
                                            ""
                                        )
                                        (if model.isRemainPlace then
                                            model.balance.placeElementId

                                         else
                                            ""
                                        )
                                        (if model.isRemainDate then
                                            model.balance.date

                                         else
                                            ""
                                        )
                            in
                            ( { model | errorMessage = Just "OK", balance = stringBalance }, Cmd.none )

                        _ ->
                            ( model, Navigation.pushUrl model.key (Route.toPath Route.BalanceTable) )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        CheckRemainAmount bool ->
            ( { model | isRemainAmount = bool }, Cmd.none )

        CheckRemainItem bool ->
            ( { model | isRemainItem = bool }, Cmd.none )

        CheckRemainKind bool ->
            ( { model | isRemainKind = bool }, Cmd.none )

        CheckRemainPurpose bool ->
            ( { model | isRemainPurpose = bool }, Cmd.none )

        CheckRemainPlace bool ->
            ( { model | isRemainPlace = bool }, Cmd.none )

        CheckRemainDate bool ->
            ( { model | isRemainDate = bool }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            [ Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "項目" ]
                , Html.th [] [ Html.text "種別" ]
                , Html.th [] [ Html.text "予算" ]
                , Html.th [] [ Html.text "場所" ]
                , Html.th [] [ Html.text "日付" ]
                ]
            , Html.tr []
                [ Html.th [] []
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainAmount, onCheck CheckRemainAmount ] [] ]
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainItem, onCheck CheckRemainItem ] [] ]
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainKind, onCheck CheckRemainKind ] [] ]
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainPurpose, onCheck CheckRemainPurpose ] [] ]
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainPlace, onCheck CheckRemainPlace ] [] ]
                , Html.th [] [ Html.input [ Attributes.type_ "checkbox", Attributes.checked model.isRemainDate, onCheck CheckRemainDate ] [] ]
                ]
            , Html.tr []
                [ Html.td []
                    [ Html.text
                        (case model.id of
                            Nothing ->
                                "+"

                            Just id ->
                                String.fromInt id
                        )
                    ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.balance.amount, onInput InputAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.balance.item, onInput InputItem ] [] ]
                , Html.td []
                    [ Html.select [ onInput InputKindElementId, Attributes.value model.balance.kindElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.balance.kindElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.kindElements
                        )
                    ]
                , Html.td []
                    [ Html.select [ onInput InputPurposeElementId, Attributes.value model.balance.purposeElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.balance.purposeElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.purposeElements
                        )
                    ]
                , Html.td []
                    [ Html.select [ onInput InputPlaceElementId, Attributes.value model.balance.placeElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.balance.placeElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.placeElements
                        )
                    ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.balance.date, onInput InputDate ] [] ]
                ]
            ]
        , Html.div []
            (case model.id of
                Nothing ->
                    [ Html.button [ onClick Upsert ] [ Html.text "作成" ] ]

                Just moveId ->
                    [ Html.button [ onClick Upsert ] [ Html.text "保存" ]
                    , Html.button [ onClick (Delete moveId) ] [ Html.text "削除" ]
                    , Html.input [ Attributes.type_ "text", Attributes.value model.deleteString, onInput InputDeleteString, Attributes.hidden (not model.enableInputDeleteString) ] []
                    ]
            )
        , Html.div []
            [ Html.button [ onClick Cancel ] [ Html.text "キャンセル" ] ]
        ]
