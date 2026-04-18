module Page.TemplateTable exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import List
import Maybe
import Model.Enitity.BalanceEntity as BalanceEntity
import Model.Enitity.MoveEntity as MoveEntity
import Model.Enitity.TemplateEntity as TemplateEntity
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Request.Request as Request
import Route
import String


type alias Selected =
    { template : TemplateEntity.Template
    , details : List TemplateEntity.TemplateDetail
    , amounts : List String
    , date : String
    }


type alias Model =
    { templates : TemplateEntity.Templates
    , openTemplates : Dict Int Selected
    , isDisabledEditButton : Bool
    , errorMessage : Maybe String
    }


type Msg
    = GetTemplates (Result Request.Error TemplateEntity.Templates)
    | SelectTemplate Int
    | GetTemplate (Result Request.Error ( TemplateEntity.Template, List TemplateEntity.TemplateDetail ))
    | InputDate Int String
    | InputAmount Int Int String
    | Submit Int
    | GotFirstGroupId Int String (List ( Int, TemplateEntity.TemplateDetail )) (Result Request.Error Int)
    | PostedSubsequent Int Int String (List ( Int, TemplateEntity.TemplateDetail )) (Result Request.Error ())


init : ( Model, Cmd Msg )
init =
    ( Model [] Dict.empty False Nothing
    , Request.getTemplates GetTemplates
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTemplates result ->
            case result of
                Ok response ->
                    ( { model | templates = response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        SelectTemplate id ->
            if Dict.member id model.openTemplates then
                ( { model | openTemplates = Dict.remove id model.openTemplates }, Cmd.none )

            else
                ( model, Request.getTemplate id GetTemplate )

        GetTemplate result ->
            case result of
                Ok ( template, details ) ->
                    let
                        selected =
                            { template = template
                            , details = details
                            , amounts = List.map (\d -> String.fromInt d.amount) details
                            , date = ""
                            }
                    in
                    ( { model | openTemplates = Dict.insert template.id selected model.openTemplates }
                    , Cmd.none
                    )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        InputDate id date ->
            ( { model
                | openTemplates =
                    Dict.update id (Maybe.map (\s -> { s | date = date })) model.openTemplates
              }
            , Cmd.none
            )

        InputAmount id index amount ->
            ( { model
                | openTemplates =
                    Dict.update id
                        (Maybe.map
                            (\s ->
                                { s
                                    | amounts =
                                        List.indexedMap
                                            (\i a ->
                                                if i == index then
                                                    amount

                                                else
                                                    a
                                            )
                                            s.amounts
                                }
                            )
                        )
                        model.openTemplates
              }
            , Cmd.none
            )

        Submit id ->
            case Dict.get id model.openTemplates of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    let
                        pairs =
                            List.map2
                                (\amountStr detail ->
                                    ( String.toInt amountStr |> Maybe.withDefault detail.amount, detail )
                                )
                                selected.amounts
                                selected.details
                    in
                    case pairs of
                        [] ->
                            ( model, Cmd.none )

                        ( amount, detail ) :: rest ->
                            ( { model | isDisabledEditButton = True, errorMessage = Nothing }
                            , postFirstDetail id selected.date amount detail rest
                            )

        GotFirstGroupId templateId date remaining result ->
            case result of
                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Ok groupId ->
                    case remaining of
                        [] ->
                            ( { model | errorMessage = Just "OK", isDisabledEditButton = False }, Cmd.none )

                        ( amount, detail ) :: rest ->
                            ( model, postSubsequentDetail templateId groupId date amount detail rest )

        PostedSubsequent templateId groupId date remaining result ->
            case result of
                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Ok _ ->
                    case remaining of
                        [] ->
                            ( { model | errorMessage = Just "OK", isDisabledEditButton = False }, Cmd.none )

                        ( amount, detail ) :: rest ->
                            ( model, postSubsequentDetail templateId groupId date amount detail rest )


postFirstDetail : Int -> String -> Int -> TemplateEntity.TemplateDetail -> List ( Int, TemplateEntity.TemplateDetail ) -> Cmd Msg
postFirstDetail templateId date amount detail remaining =
    let
        toMsg result =
            GotFirstGroupId templateId date remaining result
    in
    case detail.type_ of
        1 ->
            Request.postBalanceGetGroupId
                (BalanceEntity.NewBalance amount detail.item detail.kindElementId
                    (detail.purposeElementId |> Maybe.withDefault 0)
                    (detail.placeElementId |> Maybe.withDefault 0)
                    date
                    Nothing
                )
                toMsg

        2 ->
            Request.postMoveGetGroupId
                MoveAttributeValueObject.Purpose
                (MoveEntity.NewMove amount detail.item
                    (detail.moveBeforePurposeId |> Maybe.withDefault 0)
                    (detail.moveAfterPurposeId |> Maybe.withDefault 0)
                    date
                    Nothing
                )
                toMsg

        3 ->
            Request.postMoveGetGroupId
                MoveAttributeValueObject.Place
                (MoveEntity.NewMove amount detail.item
                    (detail.moveBeforePlaceId |> Maybe.withDefault 0)
                    (detail.moveAfterPlaceId |> Maybe.withDefault 0)
                    date
                    Nothing
                )
                toMsg

        _ ->
            case remaining of
                [] ->
                    Cmd.none

                ( nextAmount, nextDetail ) :: rest ->
                    postFirstDetail templateId date nextAmount nextDetail rest


postSubsequentDetail : Int -> Int -> String -> Int -> TemplateEntity.TemplateDetail -> List ( Int, TemplateEntity.TemplateDetail ) -> Cmd Msg
postSubsequentDetail templateId groupId date amount detail remaining =
    let
        toMsg result =
            PostedSubsequent templateId groupId date remaining result
    in
    case detail.type_ of
        1 ->
            Request.postBalance
                (BalanceEntity.NewBalance amount detail.item detail.kindElementId
                    (detail.purposeElementId |> Maybe.withDefault 0)
                    (detail.placeElementId |> Maybe.withDefault 0)
                    date
                    (Just groupId)
                )
                toMsg

        2 ->
            Request.postMove
                MoveAttributeValueObject.Purpose
                (MoveEntity.NewMove amount detail.item
                    (detail.moveBeforePurposeId |> Maybe.withDefault 0)
                    (detail.moveAfterPurposeId |> Maybe.withDefault 0)
                    date
                    (Just groupId)
                )
                toMsg

        3 ->
            Request.postMove
                MoveAttributeValueObject.Place
                (MoveEntity.NewMove amount detail.item
                    (detail.moveBeforePlaceId |> Maybe.withDefault 0)
                    (detail.moveAfterPlaceId |> Maybe.withDefault 0)
                    date
                    (Just groupId)
                )
                toMsg

        _ ->
            case remaining of
                [] ->
                    Cmd.none

                ( nextAmount, nextDetail ) :: rest ->
                    postSubsequentDetail templateId groupId date nextAmount nextDetail rest


typeLabel : Int -> String
typeLabel t =
    case t of
        1 ->
            "収支"

        2 ->
            "予算移動"

        3 ->
            "場所移動"

        _ ->
            "不明"


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "名前" ]
                , Html.th [] []
                ]
                :: Html.tr []
                    [ Html.td [] [ Html.a [ Attributes.href (Route.toPath Route.TemplateCreate) ] [ Html.text "+" ] ]
                    , Html.td [] []
                    , Html.td [] []
                    ]
                :: List.concatMap
                    (\template ->
                        let
                            maybeSelected =
                                Dict.get template.id model.openTemplates
                        in
                        Html.tr
                            [ onClick (SelectTemplate template.id)
                            , Attributes.style "cursor" "pointer"
                            ]
                            [ Html.td [] [ Html.a [ Attributes.href (Route.toPath (Route.TemplateId template.id)) ] [ Html.text <| String.fromInt template.id ] ]
                            , Html.td [] [ Html.text template.name ]
                            , Html.td [] []
                            ]
                            :: (case maybeSelected of
                                    Nothing ->
                                        []

                                    Just selected ->
                                        List.indexedMap
                                            (\i detail ->
                                                let
                                                    amount =
                                                        List.drop i selected.amounts
                                                            |> List.head
                                                            |> Maybe.withDefault ""
                                                in
                                                Html.tr [ Attributes.style "background-color" "#e8f4ff" ]
                                                    [ Html.td [] [ Html.text (typeLabel detail.type_) ]
                                                    , Html.td [] [ Html.text detail.item ]
                                                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value amount, onInput (InputAmount template.id i) ] [] ]
                                                    ]
                                            )
                                            selected.details
                                            ++ [ Html.tr [ Attributes.style "background-color" "#e8f4ff" ]
                                                    [ Html.td []
                                                        [ Html.input
                                                            [ Attributes.type_ "date"
                                                            , Attributes.value selected.date
                                                            , onInput (InputDate template.id)
                                                            ]
                                                            []
                                                        ]
                                                    , Html.td [ Attributes.colspan 2 ]
                                                        [ Html.button
                                                            [ Attributes.class "edit-button"
                                                            , onClick (Submit template.id)
                                                            , Attributes.disabled model.isDisabledEditButton
                                                            ]
                                                            [ Html.text "登録" ]
                                                        ]
                                                    ]
                                               ]
                               )
                    )
                    model.templates
            )
        ]
