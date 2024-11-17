module Page.ElementTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { attributeElements : AttributeElementEntity.AttributeElements
    , attributeValueObject : AttributeValueObject.Attribute
    , errorMessage : Maybe String
    }


type Msg
    = None
    | GetAttributeElements (Result Request.Error AttributeElementEntity.AttributeElements)


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

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        routing id =
            case model.attributeValueObject of
                AttributeValueObject.Kind ->
                    Route.toPath (Route.KindElementId id)

                AttributeValueObject.Purpose ->
                    Route.toPath (Route.PurposeElementId id)

                AttributeValueObject.Place ->
                    Route.toPath (Route.PlaceElementId id)
    in
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
                ]
                :: List.map
                    (\attributeElement ->
                        Html.tr []
                            [ Html.td [] [ Html.a [ Attributes.href (routing attributeElement.id) ] [ Html.text <| String.fromInt attributeElement.id ] ]
                            , Html.td [] [ Html.text attributeElement.name ]
                            , Html.td [] [ Html.text attributeElement.description ]
                            , Html.td [] [ Html.text <| String.fromInt attributeElement.priority ]
                            , Html.td [] [ Html.text <| String.fromInt attributeElement.categoryId ]
                            ]
                    )
                    model.attributeElements
            )
        ]
