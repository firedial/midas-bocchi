module Page.ElementId exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import String


type alias Model =
    { attributeElement : InitGet
    , attributeName : AttributeValueObject.Attribute
    , id : Int
    , errorMessage : Maybe String
    }


type InitGet
    = NoData
    | Success AttributeElementEntity.AttributeElement


type Msg
    = None
    | GetAttributeElement (Result Request.Error AttributeElementEntity.AttributeElement)


init : AttributeValueObject.Attribute -> Int -> ( Model, Cmd Msg )
init attributeValueObject id =
    ( Model NoData
        attributeValueObject
        id
        Nothing
    , Request.getAttributeElement attributeValueObject id GetAttributeElement
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GetAttributeElement result ->
            case result of
                Ok response ->
                    ( { model | attributeElement = Success response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.attributeElement of
        NoData ->
            Html.div []
                [ Html.text (model.errorMessage |> Maybe.withDefault "")
                ]

        Success attributeElement ->
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
                        , Html.td [] [ Html.text attributeElement.name ]
                        , Html.td [] [ Html.text attributeElement.desription ]
                        , Html.td [] [ Html.text <| String.fromInt attributeElement.priority ]
                        , Html.td [] [ Html.text <| String.fromInt attributeElement.categoryId ]
                        ]
                    ]
                ]
