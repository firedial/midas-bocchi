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


type DetailStatus
    = DetailIdle
    | DetailSuccess
    | DetailFailed
    | DetailSkipped


type SubmitStatus
    = Idle
    | Submitting
    | AllSuccess
    | FailedFirst String
    | FailedSubsequent String Int Int


type alias Selected =
    { template : TemplateEntity.Template
    , details : List TemplateEntity.TemplateDetail
    , amounts : List String
    , date : String
    , submitStatus : SubmitStatus
    , detailStatuses : List DetailStatus
    }


type alias Model =
    { templates : TemplateEntity.Templates
    , openTemplates : Dict Int Selected
    , errorMessage : Maybe String
    }


type Msg
    = GetTemplates (Result Request.Error TemplateEntity.Templates)
    | SelectTemplate Int
    | GetTemplate (Result Request.Error ( TemplateEntity.Template, List TemplateEntity.TemplateDetail ))
    | InputDate Int String
    | InputAmount Int Int String
    | Submit Int
    | Retry Int
    | GotFirstGroupId Int Int (List ( Int, Int, TemplateEntity.TemplateDetail )) (Result Request.Error Int)
    | PostedSubsequent Int Int Int (List ( Int, Int, TemplateEntity.TemplateDetail )) (Result Request.Error ())


init : ( Model, Cmd Msg )
init =
    ( Model [] Dict.empty Nothing
    , Request.getTemplates GetTemplates
    )


setDetailStatus : Int -> DetailStatus -> List DetailStatus -> List DetailStatus
setDetailStatus idx status =
    List.indexedMap (\i s -> if i == idx then status else s)


buildPairs : List String -> List TemplateEntity.TemplateDetail -> List ( Int, Int, TemplateEntity.TemplateDetail )
buildPairs amounts details =
    List.indexedMap
        (\i detail ->
            ( i
            , List.drop i amounts |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault detail.amount
            , detail
            )
        )
        details


separatePairs :
    List ( Int, Int, TemplateEntity.TemplateDetail )
    -> ( List Int, List ( Int, Int, TemplateEntity.TemplateDetail ) )
separatePairs =
    List.foldl
        (\( i, amount, detail ) ( skipped, active ) ->
            if amount == 0 then
                ( i :: skipped, active )

            else
                ( skipped, active ++ [ ( i, amount, detail ) ] )
        )
        ( [], [] )


buildInitStatuses : Int -> List Int -> List DetailStatus
buildInitStatuses total skippedIdxs =
    List.indexedMap
        (\i _ ->
            if List.member i skippedIdxs then
                DetailSkipped

            else
                DetailIdle
        )
        (List.repeat total ())


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
                            , submitStatus = Idle
                            , detailStatuses = List.map (\_ -> DetailIdle) details
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
                        ( skippedIdxs, activePairs ) =
                            separatePairs (buildPairs selected.amounts selected.details)

                        newSelected =
                            { selected
                                | submitStatus = Submitting
                                , detailStatuses = buildInitStatuses (List.length selected.details) skippedIdxs
                            }
                    in
                    case activePairs of
                        [] ->
                            ( { model | openTemplates = Dict.insert id { newSelected | submitStatus = AllSuccess } model.openTemplates }
                            , Cmd.none
                            )

                        head :: rest ->
                            ( { model | openTemplates = Dict.insert id newSelected model.openTemplates }
                            , postFirstDetail id selected.date head rest
                            )

        Retry id ->
            case Dict.get id model.openTemplates of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    case selected.submitStatus of
                        FailedSubsequent _ groupId retryFromIdx ->
                            let
                                ( skippedIdxs, activePairs ) =
                                    separatePairs (List.drop retryFromIdx (buildPairs selected.amounts selected.details))

                                newStatuses =
                                    List.indexedMap
                                        (\i s ->
                                            if i < retryFromIdx then
                                                s

                                            else if List.member i skippedIdxs then
                                                DetailSkipped

                                            else
                                                DetailIdle
                                        )
                                        selected.detailStatuses

                                newSelected =
                                    { selected | submitStatus = Submitting, detailStatuses = newStatuses }
                            in
                            case activePairs of
                                [] ->
                                    ( { model | openTemplates = Dict.insert id { newSelected | submitStatus = AllSuccess } model.openTemplates }
                                    , Cmd.none
                                    )

                                ( nextIdx, nextAmount, nextDetail ) :: rest ->
                                    ( { model | openTemplates = Dict.insert id newSelected model.openTemplates }
                                    , postSubsequentDetail id groupId selected.date nextIdx nextAmount nextDetail rest
                                    )

                        _ ->
                            let
                                ( skippedIdxs, activePairs ) =
                                    separatePairs (buildPairs selected.amounts selected.details)

                                newSelected =
                                    { selected
                                        | submitStatus = Submitting
                                        , detailStatuses = buildInitStatuses (List.length selected.details) skippedIdxs
                                    }
                            in
                            case activePairs of
                                [] ->
                                    ( { model | openTemplates = Dict.insert id { newSelected | submitStatus = AllSuccess } model.openTemplates }
                                    , Cmd.none
                                    )

                                head :: rest ->
                                    ( { model | openTemplates = Dict.insert id newSelected model.openTemplates }
                                    , postFirstDetail id selected.date head rest
                                    )

        GotFirstGroupId templateId firstIdx remaining result ->
            case Dict.get templateId model.openTemplates of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    case result of
                        Err (Request.DecodeError message) ->
                            let
                                newSelected =
                                    { selected
                                        | submitStatus = FailedFirst message
                                        , detailStatuses = setDetailStatus firstIdx DetailFailed selected.detailStatuses
                                    }
                            in
                            ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                        Err (Request.RequestError message) ->
                            let
                                newSelected =
                                    { selected
                                        | submitStatus = FailedFirst message
                                        , detailStatuses = setDetailStatus firstIdx DetailFailed selected.detailStatuses
                                    }
                            in
                            ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                        Ok groupId ->
                            let
                                updatedStatuses =
                                    setDetailStatus firstIdx DetailSuccess selected.detailStatuses
                            in
                            case remaining of
                                [] ->
                                    let
                                        newSelected =
                                            { selected | submitStatus = AllSuccess, detailStatuses = updatedStatuses }
                                    in
                                    ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                                ( nextIdx, nextAmount, nextDetail ) :: rest ->
                                    let
                                        newSelected =
                                            { selected | detailStatuses = updatedStatuses }
                                    in
                                    ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }
                                    , postSubsequentDetail templateId groupId selected.date nextIdx nextAmount nextDetail rest
                                    )

        PostedSubsequent templateId groupId postedIdx remaining result ->
            case Dict.get templateId model.openTemplates of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    case result of
                        Err (Request.DecodeError message) ->
                            let
                                newSelected =
                                    { selected
                                        | submitStatus = FailedSubsequent message groupId postedIdx
                                        , detailStatuses = setDetailStatus postedIdx DetailFailed selected.detailStatuses
                                    }
                            in
                            ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                        Err (Request.RequestError message) ->
                            let
                                newSelected =
                                    { selected
                                        | submitStatus = FailedSubsequent message groupId postedIdx
                                        , detailStatuses = setDetailStatus postedIdx DetailFailed selected.detailStatuses
                                    }
                            in
                            ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                        Ok _ ->
                            let
                                updatedStatuses =
                                    setDetailStatus postedIdx DetailSuccess selected.detailStatuses
                            in
                            case remaining of
                                [] ->
                                    let
                                        newSelected =
                                            { selected | submitStatus = AllSuccess, detailStatuses = updatedStatuses }
                                    in
                                    ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }, Cmd.none )

                                ( nextIdx, nextAmount, nextDetail ) :: rest ->
                                    let
                                        newSelected =
                                            { selected | detailStatuses = updatedStatuses }
                                    in
                                    ( { model | openTemplates = Dict.insert templateId newSelected model.openTemplates }
                                    , postSubsequentDetail templateId groupId selected.date nextIdx nextAmount nextDetail rest
                                    )


postFirstDetail : Int -> String -> ( Int, Int, TemplateEntity.TemplateDetail ) -> List ( Int, Int, TemplateEntity.TemplateDetail ) -> Cmd Msg
postFirstDetail templateId date ( idx, amount, detail ) remaining =
    let
        toMsg result =
            GotFirstGroupId templateId idx remaining result
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

                head :: rest ->
                    postFirstDetail templateId date head rest


postSubsequentDetail : Int -> Int -> String -> Int -> Int -> TemplateEntity.TemplateDetail -> List ( Int, Int, TemplateEntity.TemplateDetail ) -> Cmd Msg
postSubsequentDetail templateId groupId date idx amount detail remaining =
    let
        toMsg result =
            PostedSubsequent templateId groupId idx remaining result
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

                ( nextIdx, nextAmount, nextDetail ) :: rest ->
                    postSubsequentDetail templateId groupId date nextIdx nextAmount nextDetail rest


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


viewDetailStatus : DetailStatus -> Html.Html Msg
viewDetailStatus status =
    case status of
        DetailIdle ->
            Html.span [ Attributes.style "color" "gray" ] [ Html.text "未送信" ]

        DetailSuccess ->
            Html.span [ Attributes.style "color" "green" ] [ Html.text "✓" ]

        DetailFailed ->
            Html.span [ Attributes.style "color" "red" ] [ Html.text "✗" ]

        DetailSkipped ->
            Html.span [ Attributes.style "color" "gray" ] [ Html.text "スキップ" ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "名前" ]
                , Html.th [] []
                , Html.th [] []
                ]
                :: Html.tr []
                    [ Html.td [] [ Html.a [ Attributes.href (Route.toPath Route.TemplateCreate) ] [ Html.text "+" ] ]
                    , Html.td [] []
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

                                                    detailStatus =
                                                        List.drop i selected.detailStatuses
                                                            |> List.head
                                                            |> Maybe.withDefault DetailIdle
                                                in
                                                Html.tr [ Attributes.style "background-color" "#e8f4ff" ]
                                                    [ Html.td [] [ Html.text (typeLabel detail.type_) ]
                                                    , Html.td [] [ Html.text detail.item ]
                                                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value amount, onInput (InputAmount template.id i) ] [] ]
                                                    , Html.td [] [ viewDetailStatus detailStatus ]
                                                    ]
                                            )
                                            selected.details
                                            ++ [ Html.tr [ Attributes.style "background-color" "#e8f4ff" ]
                                                    [ Html.td [ Attributes.colspan 2 ]
                                                        [ case selected.submitStatus of
                                                            Submitting ->
                                                                Html.text "登録中..."

                                                            AllSuccess ->
                                                                Html.span [ Attributes.style "color" "green" ] [ Html.text "OK" ]

                                                            FailedFirst message ->
                                                                Html.span [ Attributes.style "color" "red" ] [ Html.text message ]

                                                            FailedSubsequent message _ _ ->
                                                                Html.span [ Attributes.style "color" "red" ] [ Html.text message ]

                                                            _ ->
                                                                Html.text ""
                                                        ]
                                                    , Html.td []
                                                        [ Html.input
                                                            [ Attributes.type_ "date"
                                                            , Attributes.value selected.date
                                                            , onInput (InputDate template.id)
                                                            ]
                                                            []
                                                        ]
                                                    , Html.td []
                                                        [ case selected.submitStatus of
                                                            FailedSubsequent _ _ retryFromIdx ->
                                                                Html.button
                                                                    [ onClick (Retry template.id) ]
                                                                    [ Html.text ("再送 (" ++ String.fromInt (List.length selected.details - retryFromIdx) ++ "件)") ]

                                                            _ ->
                                                                Html.button
                                                                    [ Attributes.class "edit-button"
                                                                    , onClick (Submit template.id)
                                                                    , Attributes.disabled (selected.submitStatus == Submitting)
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
