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
    { move : Maybe StringMove
    , attributeElements : Maybe AttributeElementEntity.AttributeElements
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
    = None
    | InputAmount String
    | InputItem String
    | SelectedBeforeElementId String
    | SelectedAfterElementId String
    | InputDate String
    | GetMove (Result Request.Error MoveEntity.Move)
    | GetAttributeElements (Result Request.Error AttributeElementEntity.AttributeElements)
    | Save
    | Create
    | Cancel
    | Delete Int
    | InputDeleteString String
    | UpsertResult (Result Request.Error ())


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
        (case id of
            Nothing ->
                Just (StringMove "" "" "" "" "")

            Just _ ->
                Nothing
        )
        Nothing
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
        None ->
            ( model, Cmd.none )

        InputAmount amount ->
            let
                newMove =
                    model.move
                        |> Maybe.andThen (\move -> Just { move | amount = amount })
            in
            ( { model | move = newMove }, Cmd.none )

        InputItem item ->
            let
                newMove =
                    model.move
                        |> Maybe.andThen (\move -> Just { move | item = item })
            in
            ( { model | move = newMove }, Cmd.none )

        SelectedBeforeElementId elementId ->
            let
                newMove =
                    model.move
                        |> Maybe.andThen (\move -> Just { move | beforeId = elementId })
            in
            ( { model | move = newMove }, Cmd.none )

        SelectedAfterElementId elementId ->
            let
                newMove =
                    model.move
                        |> Maybe.andThen (\move -> Just { move | afterId = elementId })
            in
            ( { model | move = newMove }, Cmd.none )

        InputDate date ->
            let
                newMove =
                    model.move
                        |> Maybe.andThen (\move -> Just { move | date = date })
            in
            ( { model | move = newMove }, Cmd.none )

        GetMove result ->
            case result of
                Ok response ->
                    let
                        stringMove =
                            StringMove
                                (String.fromInt response.amount)
                                response.item
                                (String.fromInt response.beforeId)
                                (String.fromInt response.afterId)
                                response.date
                    in
                    ( { model | move = Just stringMove }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        GetAttributeElements result ->
            case result of
                Ok response ->
                    ( { model | attributeElements = Just response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Save ->
            case model.move of
                Nothing ->
                    ( model, Cmd.none )

                Just move ->
                    case model.id of
                        Nothing ->
                            ( model, Cmd.none )

                        Just id ->
                            let
                                requestMove =
                                    MoveEntity.Move
                                        id
                                        (String.toInt
                                            move.amount
                                            |> Maybe.withDefault 0
                                        )
                                        move.item
                                        (String.toInt
                                            move.beforeId
                                            |> Maybe.withDefault 0
                                        )
                                        (String.toInt
                                            move.afterId
                                            |> Maybe.withDefault 0
                                        )
                                        move.date
                                        ""
                                        ""
                            in
                            ( model, Request.putMove model.xsrfToken model.moveAttributeName requestMove UpsertResult )

        Create ->
            case model.move of
                Nothing ->
                    ( model, Cmd.none )

                Just move ->
                    case model.id of
                        Nothing ->
                            let
                                requestMove =
                                    MoveEntity.NewMove
                                        (String.toInt
                                            move.amount
                                            |> Maybe.withDefault 0
                                        )
                                        move.item
                                        (String.toInt
                                            move.beforeId
                                            |> Maybe.withDefault 0
                                        )
                                        (String.toInt
                                            move.afterId
                                            |> Maybe.withDefault 0
                                        )
                                        move.date
                            in
                            ( model, Request.postMove model.xsrfToken model.moveAttributeName requestMove UpsertResult )

                        Just _ ->
                            ( model, Cmd.none )

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
                ( model, Request.deleteMove model.xsrfToken model.moveAttributeName moveId UpsertResult )

            else
                ( { model | enableInputDeleteString = True }, Cmd.none )

        InputDeleteString deleteString ->
            ( { model | deleteString = deleteString }, Cmd.none )

        UpsertResult result ->
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
    case model.move of
        Nothing ->
            Html.div []
                [ Html.text (model.errorMessage |> Maybe.withDefault "")
                ]

        Just move ->
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
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value move.amount, onInput InputAmount ] [] ]
                        , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value move.item, onInput InputItem ] [] ]
                        , Html.td []
                            [ Html.select [ onInput SelectedBeforeElementId, Attributes.value <| move.beforeId ]
                                (case model.attributeElements of
                                    Nothing ->
                                        []

                                    Just attributeElements ->
                                        List.map
                                            (\attributeElement ->
                                                Html.option
                                                    [ Attributes.value <| String.fromInt attributeElement.id
                                                    , Attributes.selected (String.fromInt attributeElement.id == move.beforeId)
                                                    ]
                                                    [ Html.text <| attributeElement.description ]
                                            )
                                            attributeElements
                                )
                            ]
                        , Html.td []
                            [ Html.select [ onInput SelectedAfterElementId, Attributes.value <| move.afterId ]
                                (case model.attributeElements of
                                    Nothing ->
                                        []

                                    Just attributeElements ->
                                        List.map
                                            (\attributeElement ->
                                                Html.option
                                                    [ Attributes.value <| String.fromInt attributeElement.id
                                                    , Attributes.selected (String.fromInt attributeElement.id == move.afterId)
                                                    ]
                                                    [ Html.text <| attributeElement.description ]
                                            )
                                            attributeElements
                                )
                            ]
                        , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value move.date, onInput InputDate ] [] ]
                        ]
                    ]
                , Html.div []
                    (case model.id of
                        Nothing ->
                            [ Html.button [ onClick Create ] [ Html.text "作成" ] ]

                        Just moveId ->
                            [ Html.button [ onClick Save ] [ Html.text "保存" ]
                            , Html.button [ onClick (Delete moveId) ] [ Html.text "削除" ]
                            , Html.input [ Attributes.type_ "text", Attributes.value model.deleteString, onInput InputDeleteString, Attributes.hidden (not model.enableInputDeleteString) ] []
                            ]
                    )
                , Html.div []
                    [ Html.button [ onClick Cancel ] [ Html.text "キャンセル" ] ]
                ]