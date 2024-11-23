module Page.MoveTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.MoveEntity as MoveEntity
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { moves : MoveEntity.Moves
    , moveAttributeValueObject : MoveAttributeValueObject.Attribute
    , errorMessage : Maybe String
    }


type Msg
    = None
    | GetAttributeMoves (Result Request.Error MoveEntity.Moves)


init : MoveAttributeValueObject.Attribute -> ( Model, Cmd Msg )
init moveAttributeValueObject =
    ( Model []
        moveAttributeValueObject
        Nothing
    , Request.getMoves moveAttributeValueObject GetAttributeMoves
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GetAttributeMoves result ->
            case result of
                Ok response ->
                    ( { model | moves = response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        routing id =
            case model.moveAttributeValueObject of
                MoveAttributeValueObject.Purpose ->
                    Route.toPath (Route.PurposeMoveId id)

                MoveAttributeValueObject.Place ->
                    Route.toPath (Route.PlaceMoveId id)

        createRouting =
            case model.moveAttributeValueObject of
                MoveAttributeValueObject.Purpose ->
                    Route.toPath Route.PurposeElementCreate

                MoveAttributeValueObject.Place ->
                    Route.toPath Route.PlaceElementCreate
    in
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "概要" ]
                , Html.th [] [ Html.text "移動前" ]
                , Html.th [] [ Html.text "移動後" ]
                , Html.th [] [ Html.text "日付" ]
                ]
                :: Html.tr
                    []
                    [ Html.td [] [ Html.a [ Attributes.href createRouting ] [ Html.text "+" ] ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\move ->
                        Html.tr []
                            [ Html.td [] [ Html.a [ Attributes.href (routing move.id) ] [ Html.text <| String.fromInt move.id ] ]
                            , Html.td [] [ Html.text <| String.fromInt move.amount ]
                            , Html.td [] [ Html.text move.item ]
                            , Html.td [] [ Html.text move.beforeDescription ]
                            , Html.td [] [ Html.text move.afterDescription ]
                            , Html.td [] [ Html.text move.date ]
                            ]
                    )
                    model.moves
            )
        ]
