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
    { attributeElement : Maybe StringAttributeElement
    , attributeCategories : Maybe AttributeCategoryEntity.AttributeCategories
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
    = None
    | InputName String
    | InputDescription String
    | InputPriority String
    | SelectedCategory String
    | GetAttributeElement (Result Request.Error AttributeElementEntity.AttributeElement)
    | GetAttributeCategories (Result Request.Error AttributeCategoryEntity.AttributeCategories)
    | Save
    | Create
    | Cancel
    | UpsertResult (Result Request.Error ())


init : String -> Navigation.Key -> AttributeValueObject.Attribute -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key attributeValueObject id =
    ( Model
        (case id of
            Nothing ->
                Just (StringAttributeElement "" "" "" "")

            Just _ ->
                Nothing
        )
        Nothing
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
        None ->
            ( model, Cmd.none )

        InputName name ->
            let
                newAttributeElement =
                    model.attributeElement
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | name = name })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

        InputDescription description ->
            let
                newAttributeElement =
                    model.attributeElement
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | description = description })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

        InputPriority priority ->
            let
                newAttributeElement =
                    model.attributeElement
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | priority = priority })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

        SelectedCategory categoryId ->
            let
                newAttributeElement =
                    model.attributeElement
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | categoryId = categoryId })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

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
                    ( { model | attributeElement = Just stringAttributeElement }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        GetAttributeCategories result ->
            case result of
                Ok response ->
                    ( { model | attributeCategories = Just response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Save ->
            case model.attributeElement of
                Nothing ->
                    ( model, Cmd.none )

                Just attributeElement ->
                    case model.id of
                        Nothing ->
                            ( model, Cmd.none )

                        Just id ->
                            let
                                requestAttributeElement =
                                    AttributeElementEntity.AttributeElement
                                        id
                                        attributeElement.name
                                        attributeElement.description
                                        (String.toInt
                                            attributeElement.priority
                                            |> Maybe.withDefault 0
                                        )
                                        (String.toInt
                                            attributeElement.categoryId
                                            |> Maybe.withDefault 0
                                        )
                            in
                            ( model, Request.putAttributeElement model.xsrfToken model.attributeName requestAttributeElement UpsertResult )

        Create ->
            case model.attributeElement of
                Nothing ->
                    ( model, Cmd.none )

                Just attributeElement ->
                    case model.id of
                        Nothing ->
                            let
                                requestAttributeElement =
                                    AttributeElementEntity.NewAttributeElement
                                        attributeElement.name
                                        attributeElement.description
                                        (String.toInt
                                            attributeElement.priority
                                            |> Maybe.withDefault 0
                                        )
                                        (String.toInt
                                            attributeElement.categoryId
                                            |> Maybe.withDefault 0
                                        )
                            in
                            ( model, Request.postAttributeElement model.xsrfToken model.attributeName requestAttributeElement UpsertResult )

                        Just _ ->
                            ( model, Cmd.none )

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

        UpsertResult result ->
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
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.attributeElement of
        Nothing ->
            Html.div []
                [ Html.text (model.errorMessage |> Maybe.withDefault "")
                ]

        Just attributeElement ->
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
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value attributeElement.name, onInput InputName ] [] ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value attributeElement.description, onInput InputDescription ] [] ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value attributeElement.priority, onInput InputPriority ] [] ]
                        , Html.td []
                            [ Html.select [ onInput SelectedCategory, Attributes.value <| attributeElement.categoryId ]
                                (case model.attributeCategories of
                                    Nothing ->
                                        []

                                    Just attributeCategories ->
                                        List.map
                                            (\attributeCategory ->
                                                Html.option
                                                    [ Attributes.value <| String.fromInt attributeCategory.id
                                                    , Attributes.selected (String.fromInt attributeCategory.id == attributeElement.categoryId)
                                                    ]
                                                    [ Html.text <| attributeCategory.description ]
                                            )
                                            attributeCategories
                                )
                            ]
                        ]
                    ]
                , Html.div []
                    (case model.id of
                        Nothing ->
                            [ Html.button [ onClick Create ] [ Html.text "作成" ] ]

                        Just _ ->
                            [ Html.button [ onClick Save ] [ Html.text "保存" ] ]
                    )
                , Html.div []
                    [ Html.button [ onClick Cancel ] [ Html.text "キャンセル" ] ]
                ]
