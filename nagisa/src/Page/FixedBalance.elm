module Page.FixedBalance exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Html.Events exposing (onInput)
import List
import Maybe
import Model.Enitity.FixedBalanceEntity as FixedBalanceEntity
import Request.Request as Request
import Route
import String


type alias Model =
    { stringBalances : List StringBalance
    , errorMessage : Maybe String
    }


type alias StringBalance =
    { fixedBalanceId : String
    , amount : String
    , item : String
    , kindElementId : String
    , purposeElementId : String
    , placeElementId : String
    , kindElementDescription : String
    , purposeElementDescription : String
    , placeElementDescription : String
    , date : String
    }


type Msg
    = GetFixedBalances (Result Request.Error FixedBalanceEntity.FixedBalances)
    | InputAmount String String
    | InputDate String String


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
                    let
                        stringBalances =
                            List.map
                                (\fixedBalance ->
                                    StringBalance
                                        (String.fromInt fixedBalance.fixedBalanceId)
                                        (String.fromInt fixedBalance.amount)
                                        fixedBalance.item
                                        (String.fromInt fixedBalance.kindElementId)
                                        (String.fromInt fixedBalance.purposeElementId)
                                        (String.fromInt fixedBalance.placeElementId)
                                        fixedBalance.kindElementDescription
                                        fixedBalance.purposeElementDescription
                                        fixedBalance.placeElementDescription
                                        ""
                                )
                                response
                    in
                    ( { model | stringBalances = stringBalances }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        InputAmount fixedBalanceId amount ->
            let
                newStringBalances =
                    List.map
                        (\stringBalance ->
                            if stringBalance.fixedBalanceId == fixedBalanceId then
                                { stringBalance | amount = amount }

                            else
                                stringBalance
                        )
                        model.stringBalances
            in
            ( { model | stringBalances = newStringBalances }, Cmd.none )

        InputDate fixedBalanceId date ->
            let
                newStringBalances =
                    List.map
                        (\stringBalance ->
                            if stringBalance.fixedBalanceId == fixedBalanceId then
                                { stringBalance | date = date }

                            else
                                stringBalance
                        )
                        model.stringBalances
            in
            ( { model | stringBalances = newStringBalances }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr
                []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "項目" ]
                , Html.th [] [ Html.text "種別" ]
                , Html.th [] [ Html.text "予算" ]
                , Html.th [] [ Html.text "場所" ]
                , Html.th [] [ Html.text "金額" ]
                , Html.th [] [ Html.text "日付" ]
                ]
                :: Html.tr
                    []
                    [ Html.td [] [ Html.a [ Attributes.href (Route.toPath Route.FixedBalanceCreate) ] [ Html.text "+" ] ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\stringBalance ->
                        Html.tr []
                            [ Html.td [] [ Html.a [ Attributes.href <| Route.toPath (String.toInt stringBalance.fixedBalanceId |> Maybe.withDefault 0 |> Route.FixedBalanceId) ] [ Html.text stringBalance.fixedBalanceId ] ]
                            , Html.td [] [ Html.text stringBalance.item ]
                            , Html.td [] [ Html.text stringBalance.kindElementDescription ]
                            , Html.td [] [ Html.text stringBalance.purposeElementDescription ]
                            , Html.td [] [ Html.text stringBalance.placeElementDescription ]
                            , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value stringBalance.amount, onInput (InputAmount stringBalance.fixedBalanceId) ] [] ]
                            , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value stringBalance.date, onInput (InputDate stringBalance.fixedBalanceId) ] [] ]
                            ]
                    )
                    model.stringBalances
            )
        ]
