module Page.ElementTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Request.RequestError as RequestError
import String


type alias Model =
    { attributeElements : AttributeElementEntity.AttributeElements
    , attributeName : AttributeValueObject.Attribute
    , errorMessage : Maybe String
    }


type Msg
    = None
    | GetAttributeElements (Result RequestError.Error AttributeElementEntity.AttributeElements)


init : AttributeValueObject.Attribute -> ( Model, Cmd Msg )
init attributeValueObject =
    ( Model []
        attributeValueObject
        Nothing
    , Request.getAttributeElements attributeValueObject GetAttributeElements
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GetAttributeElements result ->
            case result of
                Ok response ->
                    ( { model | attributeElements = response }, Cmd.none )

                Err (RequestError.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (RequestError.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "名前" ]
                , Html.th [] [ Html.text "概要" ]
                , Html.th [] [ Html.text "優先度" ]
                , Html.th [] [ Html.text "親id" ]
                , Html.th [] [ Html.text "編集" ]
                ]
                :: List.map
                    (\attributeElement ->
                        Html.tr []
                            [ Html.td [] [ Html.text <| String.fromInt attributeElement.id ]
                            , Html.td [] [ Html.text attributeElement.name ]
                            , Html.td [] [ Html.text attributeElement.desription ]
                            , Html.td [] [ Html.text <| String.fromInt attributeElement.priority ]
                            , Html.td [] [ Html.text <| String.fromInt attributeElement.categoryId ]
                            , Html.td [] [ Html.text "編集" ]
                            ]
                    )
                    model.attributeElements
            )
        ]
