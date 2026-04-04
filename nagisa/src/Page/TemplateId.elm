module Page.TemplateId exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import List
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.TemplateEntity as TemplateEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route
import String


type alias StringDetail =
    { type_ : String
    , amount : String
    , item : String
    , kindElementId : String
    , purposeElementId : String
    , placeElementId : String
    , moveBeforePurposeId : String
    , moveAfterPurposeId : String
    , moveBeforePlaceId : String
    , moveAfterPlaceId : String
    }


type alias Model =
    { name : String
    , details : List StringDetail
    , kindElements : AttributeElementEntity.AttributeElements
    , purposeElements : AttributeElementEntity.AttributeElements
    , placeElements : AttributeElementEntity.AttributeElements
    , id : Maybe Int
    , xsrfToken : String
    , key : Navigation.Key
    , isDisabledEditButton : Bool
    , errorMessage : Maybe String
    }


emptyDetail : StringDetail
emptyDetail =
    StringDetail "1" "0" "" "" "" "" "" "" "" ""


type Msg
    = InputName String
    | InputDetailType Int String
    | InputDetailAmount Int String
    | InputDetailItem Int String
    | InputDetailKindElementId Int String
    | InputDetailPurposeElementId Int String
    | InputDetailPlaceElementId Int String
    | InputDetailMoveBeforePurposeId Int String
    | InputDetailMoveAfterPurposeId Int String
    | InputDetailMoveBeforePlaceId Int String
    | InputDetailMoveAfterPlaceId Int String
    | AddDetail
    | RemoveDetail Int
    | GetTemplate (Result Request.Error ( TemplateEntity.Template, List TemplateEntity.TemplateDetail ))
    | GetAttributeElements AttributeValueObject.Attribute (Result Request.Error AttributeElementEntity.AttributeElements)
    | Upsert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key id =
    ( Model "" [ emptyDetail ] [] [] [] id xsrfToken key False Nothing
    , Cmd.batch
        ([ Request.getAttributeElements AttributeValueObject.Kind (GetAttributeElements AttributeValueObject.Kind)
         , Request.getAttributeElements AttributeValueObject.Purpose (GetAttributeElements AttributeValueObject.Purpose)
         , Request.getAttributeElements AttributeValueObject.Place (GetAttributeElements AttributeValueObject.Place)
         ]
            ++ (case id of
                    Nothing ->
                        []

                    Just id_ ->
                        [ Request.getTemplate id_ GetTemplate ]
               )
        )
    )


updateDetailAt : Int -> (StringDetail -> StringDetail) -> List StringDetail -> List StringDetail
updateDetailAt index f details =
    List.indexedMap
        (\i d ->
            if i == index then
                f d

            else
                d
        )
        details


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName name ->
            ( { model | name = name }, Cmd.none )

        InputDetailType i val ->
            ( { model | details = updateDetailAt i (\d -> { d | type_ = val }) model.details }, Cmd.none )

        InputDetailAmount i val ->
            ( { model | details = updateDetailAt i (\d -> { d | amount = val }) model.details }, Cmd.none )

        InputDetailItem i val ->
            ( { model | details = updateDetailAt i (\d -> { d | item = val }) model.details }, Cmd.none )

        InputDetailKindElementId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | kindElementId = val }) model.details }, Cmd.none )

        InputDetailPurposeElementId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | purposeElementId = val }) model.details }, Cmd.none )

        InputDetailPlaceElementId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | placeElementId = val }) model.details }, Cmd.none )

        InputDetailMoveBeforePurposeId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | moveBeforePurposeId = val }) model.details }, Cmd.none )

        InputDetailMoveAfterPurposeId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | moveAfterPurposeId = val }) model.details }, Cmd.none )

        InputDetailMoveBeforePlaceId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | moveBeforePlaceId = val }) model.details }, Cmd.none )

        InputDetailMoveAfterPlaceId i val ->
            ( { model | details = updateDetailAt i (\d -> { d | moveAfterPlaceId = val }) model.details }, Cmd.none )

        AddDetail ->
            ( { model | details = model.details ++ [ emptyDetail ] }, Cmd.none )

        RemoveDetail i ->
            ( { model | details = List.indexedMap Tuple.pair model.details |> List.filter (\( idx, _ ) -> idx /= i) |> List.map Tuple.second }, Cmd.none )

        GetTemplate result ->
            case result of
                Ok ( template, details ) ->
                    ( { model
                        | name = template.name
                        , details = List.map detailToString details
                      }
                    , Cmd.none
                    )

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
                newTemplate =
                    TemplateEntity.NewTemplate
                        model.name
                        (List.map buildNewDetail model.details)

                cmd =
                    case model.id of
                        Nothing ->
                            Request.postTemplate model.xsrfToken newTemplate ModifiedResult

                        Just id ->
                            Request.putTemplate model.xsrfToken id newTemplate ModifiedResult
            in
            ( { model | isDisabledEditButton = True, errorMessage = Nothing }, cmd )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.TemplateTable) )

        ModifiedResult result ->
            case result of
                Ok _ ->
                    ( model, Navigation.pushUrl model.key (Route.toPath Route.TemplateTable) )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )


detailToString : TemplateEntity.TemplateDetail -> StringDetail
detailToString d =
    StringDetail
        (String.fromInt d.type_)
        (String.fromInt d.amount)
        d.item
        (String.fromInt d.kindElementId)
        (d.purposeElementId |> Maybe.map String.fromInt |> Maybe.withDefault "")
        (d.placeElementId |> Maybe.map String.fromInt |> Maybe.withDefault "")
        (d.moveBeforePurposeId |> Maybe.map String.fromInt |> Maybe.withDefault "")
        (d.moveAfterPurposeId |> Maybe.map String.fromInt |> Maybe.withDefault "")
        (d.moveBeforePlaceId |> Maybe.map String.fromInt |> Maybe.withDefault "")
        (d.moveAfterPlaceId |> Maybe.map String.fromInt |> Maybe.withDefault "")


buildNewDetail : StringDetail -> TemplateEntity.NewTemplateDetail
buildNewDetail d =
    let
        type_ =
            String.toInt d.type_ |> Maybe.withDefault 1
    in
    TemplateEntity.NewTemplateDetail
        type_
        (String.toInt d.amount |> Maybe.withDefault 0)
        d.item
        (String.toInt d.kindElementId |> Maybe.withDefault 0)
        (if type_ == 1 then String.toInt d.purposeElementId else Nothing)
        (if type_ == 1 then String.toInt d.placeElementId else Nothing)
        (if type_ == 2 then String.toInt d.moveBeforePurposeId else Nothing)
        (if type_ == 2 then String.toInt d.moveAfterPurposeId else Nothing)
        (if type_ == 3 then String.toInt d.moveBeforePlaceId else Nothing)
        (if type_ == 3 then String.toInt d.moveAfterPlaceId else Nothing)


elementSelect : List AttributeElementEntity.AttributeElement -> String -> (String -> Msg) -> Html.Html Msg
elementSelect elements currentVal toMsg =
    Html.select [ onInput toMsg, Attributes.value currentVal ]
        (List.map
            (\e ->
                Html.option
                    [ Attributes.value <| String.fromInt e.id
                    , Attributes.selected (String.fromInt e.id == currentVal)
                    ]
                    [ Html.text e.description ]
            )
            elements
        )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.div []
            [ Html.span [] [ Html.text "名前: " ]
            , Html.input [ Attributes.type_ "text", Attributes.value model.name, onInput InputName ] []
            ]
        , Html.table [ Attributes.class "balance" ]
            (Html.tr []
                [ Html.th [] [ Html.text "種別" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "項目" ]
                , Html.th [] [ Html.text "種類" ]
                , Html.th [] [ Html.text "予算(収支)" ]
                , Html.th [] [ Html.text "場所(収支)" ]
                , Html.th [] [ Html.text "移動前予算" ]
                , Html.th [] [ Html.text "移動後予算" ]
                , Html.th [] [ Html.text "移動前場所" ]
                , Html.th [] [ Html.text "移動後場所" ]
                , Html.th [] []
                ]
                :: List.indexedMap (viewDetailRow (List.length model.details) model) model.details
            )
        , Html.button [ onClick AddDetail ] [ Html.text "+ 明細追加" ]
        , Html.div []
            (case model.id of
                Nothing ->
                    [ Html.button [ Attributes.class "edit-button", onClick Upsert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "作成" ] ]

                Just _ ->
                    [ Html.button [ Attributes.class "edit-button", onClick Upsert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "保存" ] ]
            )
        , Html.div []
            [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
        ]


viewDetailRow : Int -> Model -> Int -> StringDetail -> Html.Html Msg
viewDetailRow total model i d =
    let
        type_ =
            String.toInt d.type_ |> Maybe.withDefault 1
    in
    Html.tr []
        [ Html.td []
            [ Html.select [ onInput (InputDetailType i), Attributes.value d.type_ ]
                [ Html.option [ Attributes.value "1", Attributes.selected (type_ == 1) ] [ Html.text "収支" ]
                , Html.option [ Attributes.value "2", Attributes.selected (type_ == 2) ] [ Html.text "予算移動" ]
                , Html.option [ Attributes.value "3", Attributes.selected (type_ == 3) ] [ Html.text "場所移動" ]
                ]
            ]
        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value d.amount, onInput (InputDetailAmount i) ] [] ]
        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value d.item, onInput (InputDetailItem i) ] [] ]
        , Html.td [] [ elementSelect model.kindElements d.kindElementId (InputDetailKindElementId i) ]
        , Html.td []
            (if type_ == 1 then
                [ elementSelect model.purposeElements d.purposeElementId (InputDetailPurposeElementId i) ]

             else
                []
            )
        , Html.td []
            (if type_ == 1 then
                [ elementSelect model.placeElements d.placeElementId (InputDetailPlaceElementId i) ]

             else
                []
            )
        , Html.td []
            (if type_ == 2 then
                [ elementSelect model.purposeElements d.moveBeforePurposeId (InputDetailMoveBeforePurposeId i) ]

             else
                []
            )
        , Html.td []
            (if type_ == 2 then
                [ elementSelect model.purposeElements d.moveAfterPurposeId (InputDetailMoveAfterPurposeId i) ]

             else
                []
            )
        , Html.td []
            (if type_ == 3 then
                [ elementSelect model.placeElements d.moveBeforePlaceId (InputDetailMoveBeforePlaceId i) ]

             else
                []
            )
        , Html.td []
            (if type_ == 3 then
                [ elementSelect model.placeElements d.moveAfterPlaceId (InputDetailMoveAfterPlaceId i) ]

             else
                []
            )
        , Html.td []
            (if total > 1 then
                [ Html.button [ onClick (RemoveDetail i) ] [ Html.text "-" ] ]

             else
                []
            )
        ]
