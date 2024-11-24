module Page.MoveId exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.Enitity.MoveEntity as MoveEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Request.Request as Request
import Route
import String


type alias Model =
    { move : StringMove
    , attributeElements : AttributeElementEntity.AttributeElements
    , xsrfToken : String
    , moveAttributeName : MoveAttributeValueObject.Attribute
    , id : Maybe Int
    , enableInputDeleteString : Bool
    , deleteString : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringMove =
    { amount : String
    , item : String
    , beforeId : String
    , afterId : String
    , date : String
    }


type Msg
    = InputAmount String
    | InputItem String
    | InputBeforeElementId String
    | InputAfterElementId String
    | InputDate String
    | GetMove (Result Request.Error MoveEntity.Move)
    | GetAttributeElements (Result Request.Error AttributeElementEntity.AttributeElements)
    | Upsert
    | Cancel
    | Delete Int
    | InputDeleteString String
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> MoveAttributeValueObject.Attribute -> Maybe Int -> ( Model, Cmd Msg )
init xsrfToken key moveAttributeValueObject id =
    let
        attributeValueObject =
            case moveAttributeValueObject of
                MoveAttributeValueObject.Purpose ->
                    AttributeValueObject.Purpose

                MoveAttributeValueObject.Place ->
                    AttributeValueObject.Place
    in
    ( Model
        (StringMove "" "" "" "" "")
        []
        xsrfToken
        moveAttributeValueObject
        id
        False
        ""
        key
        Nothing
    , Cmd.batch
        (case id of
            Nothing ->
                [ Request.getAttributeElements attributeValueObject GetAttributeElements
                ]

            Just id_ ->
                [ Request.getMove moveAttributeValueObject id_ GetMove
                , Request.getAttributeElements attributeValueObject GetAttributeElements
                ]
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputAmount amount ->
            let
                newMove =
                    model.move
            in
            ( { model | move = { newMove | amount = amount } }, Cmd.none )

        InputItem item ->
            let
                newMove =
                    model.move
            in
            ( { model | move = { newMove | item = item } }, Cmd.none )

        InputBeforeElementId elementId ->
            let
                newMove =
                    model.move
            in
            ( { model | move = { newMove | beforeId = elementId } }, Cmd.none )

        InputAfterElementId elementId ->
            let
                newMove =
                    model.move
            in
            ( { model | move = { newMove | afterId = elementId } }, Cmd.none )

        InputDate date ->
            let
                newMove =
                    model.move
            in
            ( { model | move = { newMove | date = date } }, Cmd.none )

        GetMove result ->
            case result of
                Ok move ->
                    let
                        stringMove =
                            StringMove
                                (String.fromInt move.amount)
                                move.item
                                (String.fromInt move.beforeId)
                                (String.fromInt move.afterId)
                                move.date
                    in
                    ( { model | move = stringMove }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        GetAttributeElements result ->
            case result of
                Ok attributeElements ->
                    ( { model | attributeElements = attributeElements }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Upsert ->
            let
                newMove =
                    MoveEntity.NewMove
                        (String.toInt
                            model.move.amount
                            |> Maybe.withDefault 0
                        )
                        model.move.item
                        (String.toInt
                            model.move.beforeId
                            |> Maybe.withDefault 0
                        )
                        (String.toInt
                            model.move.afterId
                            |> Maybe.withDefault 0
                        )
                        model.move.date

                cmd =
                    case model.id of
                        Nothing ->
                            Request.postMove model.xsrfToken model.moveAttributeName newMove ModifiedResult

                        Just id ->
                            Request.putMove model.xsrfToken model.moveAttributeName id newMove ModifiedResult
            in
            ( model, cmd )

        Cancel ->
            let
                redirectRouting =
                    case model.moveAttributeName of
                        MoveAttributeValueObject.Purpose ->
                            Route.toPath Route.PurposeMoveTable

                        MoveAttributeValueObject.Place ->
                            Route.toPath Route.PlaceMoveTable
            in
            ( model, Navigation.pushUrl model.key redirectRouting )

        Delete moveId ->
            if model.deleteString == "delete" then
                ( model, Request.deleteMove model.xsrfToken model.moveAttributeName moveId ModifiedResult )

            else
                ( { model | enableInputDeleteString = True }, Cmd.none )

        InputDeleteString deleteString ->
            ( { model | deleteString = deleteString }, Cmd.none )

        ModifiedResult result ->
            let
                redirectRouting =
                    case model.moveAttributeName of
                        MoveAttributeValueObject.Purpose ->
                            Route.toPath Route.PurposeMoveTable

                        MoveAttributeValueObject.Place ->
                            Route.toPath Route.PlaceMoveTable
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
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            [ Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "概要" ]
                , Html.th [] [ Html.text "移動前" ]
                , Html.th [] [ Html.text "移動後" ]
                , Html.th [] [ Html.text "日付" ]
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
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.move.amount, onInput InputAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.move.item, onInput InputItem ] [] ]
                , Html.td []
                    [ Html.select [ onInput InputBeforeElementId, Attributes.value <| model.move.beforeId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.move.beforeId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.attributeElements
                        )
                    ]
                , Html.td []
                    [ Html.select [ onInput InputAfterElementId, Attributes.value <| model.move.afterId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.move.afterId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.attributeElements
                        )
                    ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.move.date, onInput InputDate ] [] ]
                ]
            ]
        , Html.div []
            (case model.id of
                Nothing ->
                    [ Html.button [ onClick Upsert ] [ Html.text "作成" ] ]

                Just moveId ->
                    [ Html.button [ onClick Upsert ] [ Html.text "保存" ]
                    , Html.button [ onClick (Delete moveId) ] [ Html.text "削除" ]
                    , Html.input [ Attributes.type_ "text", Attributes.value model.deleteString, onInput InputDeleteString, Attributes.hidden (not model.enableInputDeleteString) ] []
                    ]
            )
        , Html.div []
            [ Html.button [ onClick Cancel ] [ Html.text "キャンセル" ] ]
        ]
