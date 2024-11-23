module Page.BalanceTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.BalanceEntity as BalanceEntity
import Request.Request as Request
import Route
import String


type alias Model =
    { balances : BalanceEntity.Balances
    , errorMessage : Maybe String
    }


type Msg
    = None
    | GetBalances (Result Request.Error BalanceEntity.Balances)


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing
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

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


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
                ]
                :: Html.tr
                    []
                    [ Html.td [] [ Html.text "+" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\balance ->
                        Html.tr []
                            [ Html.td [] [ Html.a [ Attributes.href <| Route.toPath (Route.BalanceId balance.balanceId) ] [ Html.text <| String.fromInt balance.balanceId ] ]
                            , Html.td [] [ Html.text <| String.fromInt balance.amount ]
                            , Html.td [] [ Html.text balance.item ]
                            , Html.td [] [ Html.text balance.kindElementDescription ]
                            , Html.td [] [ Html.text balance.purposeElementDescription ]
                            , Html.td [] [ Html.text balance.placeElementDescription ]
                            , Html.td [] [ Html.text balance.date ]
                            ]
                    )
                    model.balances
            )
        ]
