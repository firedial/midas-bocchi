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
    , newBalance : NewBalance
    , errorMessage : Maybe String
    }


type alias NewBalance =
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
    | InputAmount String
    | InputItem String
    | InputKindElementId String
    | InputPurposeElementId String
    | InputPlaceElementId String
    | InputDate String
    | Save


init : ( Model, Cmd Msg )
init =
    ( Model []
        (NewBalance
            ""
            ""
            ""
            ""
            ""
            ""
        )
        Nothing
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

        InputAmount amount ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | amount = amount } }, Cmd.none )

        InputItem item ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | item = item } }, Cmd.none )

        InputKindElementId id ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | kindElementId = id } }, Cmd.none )

        InputPurposeElementId id ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | purposeElementId = id } }, Cmd.none )

        InputPlaceElementId id ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | placeElementId = id } }, Cmd.none )

        InputDate date ->
            let
                newBalance =
                    model.newBalance
            in
            ( { model | newBalance = { newBalance | date = date } }, Cmd.none )

        Save ->
            ( { model | newBalance = NewBalance "" "" "" "" "" "" }, Cmd.none )


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
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.amount, onInput InputAmount ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.item, onInput InputItem ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.kindElementId, onInput InputKindElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.purposeElementId, onInput InputPurposeElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.placeElementId, onInput InputPlaceElementId ] [] ]
                    , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.newBalance.date, onInput InputDate ] [] ]
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
                            , Html.td [] [ Html.text "削除" ]
                            ]
                    )
                    model.balances
            )
        ]
