module Page.ElementId exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Model.Enitity.AttributeCategoryEntity as AttributeCategoryEntity
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import String


type alias Model =
    { attributeElement : Maybe StringAttributeElement
    , attributeCategories : Maybe AttributeCategoryEntity.AttributeCategories
    , xsrfToken : String
    , attributeName : AttributeValueObject.Attribute
    , id : Int
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
    | PutElement (Result Request.Error ())


init : String -> AttributeValueObject.Attribute -> Int -> ( Model, Cmd Msg )
init xsrfToken attributeValueObject id =
    ( Model
        Nothing
        Nothing
        xsrfToken
        attributeValueObject
        id
        Nothing
    , Cmd.batch
        [ Request.getAttributeElement attributeValueObject id GetAttributeElement
        , Request.getAttributeCategories attributeValueObject GetAttributeCategories
        ]
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
                    let
                        requestAttributeElement =
                            AttributeElementEntity.AttributeElement
                                model.id
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
                    ( model, Request.putAttributeElement model.xsrfToken model.attributeName requestAttributeElement PutElement )

        PutElement result ->
            case result of
                Ok _ ->
                    ( { model | errorMessage = Just "OK" }, Cmd.none )

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
                        [ Html.td [] [ Html.text <| String.fromInt model.id ]
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
                , Html.td [] [ Html.button [ onClick Save ] [ Html.text "保存" ] ]
                ]
