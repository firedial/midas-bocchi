module Page.FixedBalance exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.FixedBalanceEntity as FixedBalanceEntity
import Request.Request as Request
import Route
import String


type alias Model =
    { fixedBalances : FixedBalanceEntity.FixedBalances
    , errorMessage : Maybe String
    }


type Msg
    = GetFixedBalances (Result Request.Error FixedBalanceEntity.FixedBalances)


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing
    , Request.getFixedBalances GetFixedBalances
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetFixedBalances result ->
            case result of
                Ok response ->
                    ( { model | fixedBalances = response }, Cmd.none )

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
                ]
                :: Html.tr
                    []
                    -- todo リンク先を変える
                    [ Html.td [] [ Html.a [ Attributes.href (Route.toPath Route.BalanceCreate) ] [ Html.text "+" ] ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\fixedBalance ->
                        Html.tr []
                            -- todo リンク先を変える
                            [ Html.td [] [ Html.a [ Attributes.href <| Route.toPath (Route.BalanceId fixedBalance.fixedBalanceId) ] [ Html.text <| String.fromInt fixedBalance.fixedBalanceId ] ]
                            , Html.td [] [ Html.text <| String.fromInt fixedBalance.amount ]
                            , Html.td [] [ Html.text fixedBalance.item ]
                            , Html.td [] [ Html.text fixedBalance.kindElementDescription ]
                            , Html.td [] [ Html.text fixedBalance.purposeElementDescription ]
                            , Html.td [] [ Html.text fixedBalance.placeElementDescription ]
                            ]
                    )
                    model.fixedBalances
            )
        ]
