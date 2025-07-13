module Page.ElementId exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Model.Enitity.AttributeCategoryEntity as AttributeCategoryEntity
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { attributeElement : StringAttributeElement
    , attributeCategories : AttributeCategoryEntity.AttributeCategories
    , isDisabledEditButton : Bool
    , xsrfToken : String
    , attributeName : AttributeValueObject.Attribute
    , id : Maybe Int
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringAttributeElement =
    { name : String
    , description : String
    , priority : String
    , categoryId : String
    }


type Msg
    = InputName String
    | InputDescription String
    | InputPriority String
    | InputCategory String
    | GetAttributeElement (Result Request.Error AttributeElementEntity.AttributeElement)
    | GetAttributeCategories (Result Request.Error AttributeCategoryEntity.AttributeCategories)
    | Upsert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> AttributeValueObject.Attribute -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key attributeValueObject id =
    ( Model
        (StringAttributeElement "" "" "" "")
        []
        False
        xsrfToken
        attributeValueObject
        id
        key
        Nothing
    , Cmd.batch
        (case id of
            Nothing ->
                [ Request.getAttributeCategories attributeValueObject GetAttributeCategories
                ]

            Just id_ ->
                [ Request.getAttributeElement attributeValueObject id_ GetAttributeElement
                , Request.getAttributeCategories attributeValueObject GetAttributeCategories
                ]
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName name ->
            let
                newAttributeElement =
                    model.attributeElement
            in
            ( { model | attributeElement = { newAttributeElement | name = name } }, Cmd.none )

        InputDescription description ->
            let
                newAttributeElement =
                    model.attributeElement
            in
            ( { model | attributeElement = { newAttributeElement | description = description } }, Cmd.none )

        InputPriority priority ->
            let
                newAttributeElement =
                    model.attributeElement
            in
            ( { model | attributeElement = { newAttributeElement | priority = priority } }, Cmd.none )

        InputCategory categoryId ->
            let
                newAttributeElement =
                    model.attributeElement
            in
            ( { model | attributeElement = { newAttributeElement | categoryId = categoryId } }, Cmd.none )

        GetAttributeElement result ->
            case result of
                Ok response ->
                    let
                        stringAttributeElement =
                            StringAttributeElement
                                response.name
                                response.description
                                (String.fromInt response.priority)
                                (String.fromInt response.categoryId)
                    in
                    ( { model | attributeElement = stringAttributeElement }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        GetAttributeCategories result ->
            case result of
                Ok response ->
                    ( { model | attributeCategories = response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Upsert ->
            let
                newAttributeElement =
                    AttributeElementEntity.NewAttributeElement
                        model.attributeElement.name
                        model.attributeElement.description
                        (String.toInt
                            model.attributeElement.priority
                            |> Maybe.withDefault 0
                        )
                        (String.toInt
                            model.attributeElement.categoryId
                            |> Maybe.withDefault 0
                        )

                cmd =
                    case model.id of
                        Nothing ->
                            Request.postAttributeElement model.xsrfToken model.attributeName newAttributeElement ModifiedResult

                        Just id ->
                            Request.putAttributeElement model.xsrfToken model.attributeName id newAttributeElement ModifiedResult
            in
            ( { model | isDisabledEditButton = True, errorMessage = Nothing }, cmd )

        Cancel ->
            let
                redirectRouting =
                    case model.attributeName of
                        AttributeValueObject.Kind ->
                            Route.toPath Route.KindElementTable

                        AttributeValueObject.Purpose ->
                            Route.toPath Route.PurposeElementTable

                        AttributeValueObject.Place ->
                            Route.toPath Route.PlaceElementTable
            in
            ( model, Navigation.pushUrl model.key redirectRouting )

        ModifiedResult result ->
            let
                redirectRouting =
                    case model.attributeName of
                        AttributeValueObject.Kind ->
                            Route.toPath Route.KindElementTable

                        AttributeValueObject.Purpose ->
                            Route.toPath Route.PurposeElementTable

                        AttributeValueObject.Place ->
                            Route.toPath Route.PlaceElementTable
            in
            case result of
                Ok _ ->
                    ( model, Navigation.pushUrl model.key redirectRouting )

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
                , Html.th [] [ Html.text "名前" ]
                , Html.th [] [ Html.text "概要" ]
                , Html.th [] [ Html.text "優先度" ]
                , Html.th [] [ Html.text "親id" ]
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
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.attributeElement.name, onInput InputName ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.attributeElement.description, onInput InputDescription ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.attributeElement.priority, onInput InputPriority ] [] ]
                , Html.td []
                    [ Html.select [ onInput InputCategory, Attributes.value <| model.attributeElement.categoryId ]
                        (List.map
                            (\attributeCategory ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeCategory.id
                                    , Attributes.selected (String.fromInt attributeCategory.id == model.attributeElement.categoryId)
                                    ]
                                    [ Html.text <| attributeCategory.description ]
                            )
                            model.attributeCategories
                        )
                    ]
                ]
            ]
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
