module Page.BalanceTable exposing (Model, Msg, init, update, view)

import Enitity.BalanceEntity as BalanceEntity
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import List
import Maybe
import Request
import String


type alias Model =
    { balances : BalanceEntity.Balances
    , inputBalance : InputBalance
    , errorMessage : Maybe String
    , xsrfToken : String
    }


type alias InputBalance =
    { amount : String
    , item : String
    , kindElementId : String
    , purposeElementId : String
    , placeElementId : String
    , date : String
    }


type Msg
    = None
    | GetBalances (Result String BalanceEntity.Balances)
    | PostBalance (Result String ())
    | DeleteBalance (Result String ())
    | InputAmount String
    | InputItem String
    | InputKindElementId String
    | InputPurposeElementId String
    | InputPlaceElementId String
    | InputDate String
    | Save
    | Delete Int


init : String -> ( Model, Cmd Msg )
init xsrfToken =
    ( Model []
        (InputBalance
            ""
            ""
            ""
            ""
            ""
            ""
        )
        Nothing
        xsrfToken
    , Request.getBalances GetBalances
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GetBalances result ->
            case result of
                Ok response ->
                    ( { model | balances = response }, Cmd.none )

                Err message ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        PostBalance result ->
            case result of
                Ok _ ->
                    ( { model | inputBalance = InputBalance "" "" "" "" "" "" }, Cmd.none )

                Err message ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        DeleteBalance result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err message ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        InputAmount amount ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | amount = amount } }, Cmd.none )

        InputItem item ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | item = item } }, Cmd.none )

        InputKindElementId id ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | kindElementId = id } }, Cmd.none )

        InputPurposeElementId id ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | purposeElementId = id } }, Cmd.none )

        InputPlaceElementId id ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | placeElementId = id } }, Cmd.none )

        InputDate date ->
            let
                newBalance =
                    model.inputBalance
            in
            ( { model | inputBalance = { newBalance | date = date } }, Cmd.none )

        Save ->
            let
                newBalance =
                    BalanceEntity.NewBalance
                        (model.inputBalance.amount |> String.toInt |> Maybe.withDefault 0)
                        model.inputBalance.item
                        (model.inputBalance.kindElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.inputBalance.purposeElementId |> String.toInt |> Maybe.withDefault 0)
                        (model.inputBalance.placeElementId |> String.toInt |> Maybe.withDefault 0)
                        model.inputBalance.date
            in
            ( model, Request.postBalance model.xsrfToken newBalance PostBalance )

        -- Delete balanceId ->
        -- ( model, Request.deleteBalance model.xsrfToken balanceId DeleteBalance )
        Delete _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "項目" ]
                , Html.th [] [ Html.text "種別" ]
                , Html.th [] [ Html.text "予算" ]
                , Html.th [] [ Html.text "場所" ]
                , Html.th [] [ Html.text "日付" ]
                , Html.th [] [ Html.text "編集" ]
                , Html.th [] [ Html.text "保存" ]
                , Html.th [] [ Html.text "削除" ]
                ]
                :: Html.tr
                    []
                    [ Html.td [] [ Html.text "+" ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.amount, onInput InputAmount ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.item, onInput InputItem ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.kindElementId, onInput InputKindElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.purposeElementId, onInput InputPurposeElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.placeElementId, onInput InputPlaceElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.inputBalance.date, onInput InputDate ] [] ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.button [ onClick Save ] [ Html.text "保存" ] ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\balance ->
                        Html.tr []
                            [ Html.td [] [ Html.text <| String.fromInt balance.balanceId ]
                            , Html.td [] [ Html.text <| String.fromInt balance.amount ]
                            , Html.td [] [ Html.text balance.item ]
                            , Html.td [] [ Html.text balance.kindElementDescription ]
                            , Html.td [] [ Html.text balance.purposeElementDescription ]
                            , Html.td [] [ Html.text balance.placeElementDescription ]
                            , Html.td [] [ Html.text balance.date ]
                            , Html.td [] [ Html.text "編集" ]
                            , Html.td [] [ Html.text "保存" ]
                            , Html.td [] [ Html.button [ onClick (Delete balance.balanceId) ] [ Html.text "削除" ] ]
                            ]
                    )
                    model.balances
            )
        ]
