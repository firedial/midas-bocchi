module Page.BalanceTable exposing (Model, Msg, init, update, view)

import Enitity.BalanceEntity as BalanceEntity
import Html
import Html.Attributes
import List
import Maybe
import Request


type alias Model =
    { balances : BalanceEntity.Balances
    , errorMessage : Maybe String
    }


type Msg
    = None
    | GetBalances (Result String BalanceEntity.Balances)


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing, Request.getBalances GetBalances )


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


view : Model -> Html.Html Msg
view model =
    Html.div []
        ([ Html.text (model.errorMessage |> Maybe.withDefault "")
         , Html.a [ Html.Attributes.href "/account" ] [ Html.text "here" ]
         ]
            ++ List.map (\balance -> Html.div [] [ Html.text balance.item ]) model.balances
        )
