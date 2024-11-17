module Page.ElementId exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Html.Events exposing (onInput)
import Maybe
import Model.Enitity.AttributeCategoryEntity as AttributeCategoryEntity
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import String


type alias Model =
    { attributeElement : Maybe AttributeElementEntity.AttributeElement
    , attributeCategories : Maybe AttributeCategoryEntity.AttributeCategories
    , attributeName : AttributeValueObject.Attribute
    , id : Int
    , errorMessage : Maybe String
    }


type Msg
    = None
    | InputName String
    | InputDescription String
    | InputPriority String
    | SelectedCategory String
    | GetAttributeElement (Result Request.Error AttributeElementEntity.AttributeElement)
    | GetAttributeCategories (Result Request.Error AttributeCategoryEntity.AttributeCategories)


init : AttributeValueObject.Attribute -> Int -> ( Model, Cmd Msg )
init attributeValueObject id =
    ( Model Nothing
        Nothing
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
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | priority = String.toInt priority |> Maybe.withDefault 0 })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

        SelectedCategory id ->
            let
                newAttributeElement =
                    model.attributeElement
                        |> Maybe.andThen (\attributeElement -> Just { attributeElement | id = String.toInt id |> Maybe.withDefault 0 })
            in
            ( { model | attributeElement = newAttributeElement }, Cmd.none )

        GetAttributeElement result ->
            case result of
                Ok response ->
                    ( { model | attributeElement = Just response }, Cmd.none )

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
                        [ Html.td [] [ Html.text <| String.fromInt attributeElement.id ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value attributeElement.name, onInput InputName ] [] ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value attributeElement.description, onInput InputDescription ] [] ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value <| String.fromInt attributeElement.priority, onInput InputPriority ] [] ]
                        , Html.td []
                            [ Html.select [ onInput SelectedCategory, Attributes.value <| String.fromInt attributeElement.categoryId ]
                                (case model.attributeCategories of
                                    Nothing ->
                                        []

                                    Just attributeCategories ->
                                        List.map
                                            (\attributeCategory ->
                                                Html.option
                                                    [ Attributes.value <| String.fromInt attributeCategory.id
                                                    , Attributes.selected (attributeCategory.id == attributeElement.categoryId)
                                                    ]
                                                    [ Html.text <| attributeCategory.description ]
                                            )
                                            attributeCategories
                                )
                            ]
                        ]
                    ]
                ]
