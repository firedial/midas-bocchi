module Page.FixedBalanceId exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onCheck, onClick, onInput)
import List
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.FixedBalanceEntity as FixedBalanceEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { fixedBalance : StringFixedBalance
    , kindElements : AttributeElementEntity.AttributeElements
    , purposeElements : AttributeElementEntity.AttributeElements
    , placeElements : AttributeElementEntity.AttributeElements
    , xsrfToken : String
    , id : Maybe Int
    , enableInputDeleteString : Bool
    , deleteString : String
    , isDisabledEditButton : Bool
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringFixedBalance =
    { amount : String
    , item : String
    , kindElementId : String
    , purposeElementId : String
    , placeElementId : String
    }


type Msg
    = InputAmount String
    | InputItem String
    | InputKindElementId String
    | InputPurposeElementId String
    | InputPlaceElementId String
    | GetFixedBalance (Result Request.Error FixedBalanceEntity.FixedBalance)
    | GetAttributeElements AttributeValueObject.Attribute (Result Request.Error AttributeElementEntity.AttributeElements)
    | Upsert
    | Cancel
    | Delete Int
    | InputDeleteString String
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key id =
    ( Model
        (StringFixedBalance
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
        ""
        False
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
                        [ Request.getFixedBalance id_ GetFixedBalance
                        ]
               )
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputAmount amount ->
            let
                newFixedBalance =
                    model.fixedBalance
            in
            ( { model | fixedBalance = { newFixedBalance | amount = amount } }, Cmd.none )

        InputItem item ->
            let
                newFixedBalance =
                    model.fixedBalance
            in
            ( { model | fixedBalance = { newFixedBalance | item = item } }, Cmd.none )

        InputKindElementId id ->
            let
                newFixedBalance =
                    model.fixedBalance
            in
            ( { model | fixedBalance = { newFixedBalance | kindElementId = id } }, Cmd.none )

        InputPurposeElementId id ->
            let
                newFixedBalance =
                    model.fixedBalance
            in
            ( { model | fixedBalance = { newFixedBalance | purposeElementId = id } }, Cmd.none )

        InputPlaceElementId id ->
            let
                newFixedBalance =
                    model.fixedBalance
            in
            ( { model | fixedBalance = { newFixedBalance | placeElementId = id } }, Cmd.none )

        GetFixedBalance result ->
            case result of
                Ok fixedBalance ->
                    let
                        stringFixedBalance =
                            StringFixedBalance
                                (String.fromInt fixedBalance.amount)
                                fixedBalance.item
                                (String.fromInt fixedBalance.kindElementId)
                                (String.fromInt fixedBalance.purposeElementId)
                                (String.fromInt fixedBalance.placeElementId)
                    in
                    ( { model | fixedBalance = stringFixedBalance }, Cmd.none )

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
                newFixedBalance =
                    FixedBalanceEntity.NewFixedBalance
                        (model.fixedBalance.amount |> String.toInt |> Maybe.withDefault 0)
                        model.fixedBalance.item
                        (model.fixedBalance.kindElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.fixedBalance.purposeElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.fixedBalance.placeElementId |> String.toInt |> Maybe.withDefault 0)

                cmd =
                    case model.id of
                        Nothing ->
                            Request.postFixedBalance model.xsrfToken newFixedBalance ModifiedResult

                        Just id ->
                            Request.putFixedBalance model.xsrfToken id newFixedBalance ModifiedResult
            in
            ( { model | isDisabledEditButton = True, errorMessage = Nothing }, cmd )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.FixedBalance) )

        Delete id ->
            if model.deleteString == "delete" then
                ( model, Request.deleteFixedBalance model.xsrfToken id ModifiedResult )

            else
                ( { model | enableInputDeleteString = True }, Cmd.none )

        InputDeleteString deleteString ->
            ( { model | deleteString = deleteString }, Cmd.none )

        ModifiedResult result ->
            case result of
                Ok _ ->
                    ( model, Navigation.pushUrl model.key (Route.toPath Route.FixedBalance) )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )


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
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.fixedBalance.amount, onInput InputAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.fixedBalance.item, onInput InputItem ] [] ]
                , Html.td []
                    [ Html.select [ onInput InputKindElementId, Attributes.value model.fixedBalance.kindElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.fixedBalance.kindElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.kindElements
                        )
                    ]
                , Html.td []
                    [ Html.select [ onInput InputPurposeElementId, Attributes.value model.fixedBalance.purposeElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.fixedBalance.purposeElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.purposeElements
                        )
                    ]
                , Html.td []
                    [ Html.select [ onInput InputPlaceElementId, Attributes.value model.fixedBalance.placeElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.fixedBalance.placeElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.placeElements
                        )
                    ]
                ]
            ]
        , Html.div []
            (case model.id of
                Nothing ->
                    [ Html.button [ Attributes.class "edit-button", onClick Upsert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "作成" ] ]

                Just moveId ->
                    [ Html.button [ Attributes.class "edit-button", onClick Upsert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "保存" ]
                    , Html.button [ Attributes.class "delete-button", onClick (Delete moveId) ] [ Html.text "削除" ]
                    , Html.input [ Attributes.type_ "text", Attributes.value model.deleteString, onInput InputDeleteString, Attributes.hidden (not model.enableInputDeleteString) ] []
                    ]
            )
        , Html.div []
            [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
        ]
