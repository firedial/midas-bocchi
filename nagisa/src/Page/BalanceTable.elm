module Page.BalanceTable exposing (Model, Msg, init, update, view)

import Enitity.BalanceEntity as BalanceEntity
import Html
import Html.Attributes
import Http
import List
import Maybe


type alias Model =
    { balances : BalanceEntity.Balances
    , responseMessage : Maybe String
    }


type Msg
    = None
    | GetBalances (Result Http.Error BalanceEntity.Balances)


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing, Http.get { url = "/api/balances", expect = Http.expectJson GetBalances BalanceEntity.decodeBalances } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GetBalances result ->
            case result of
                Ok response ->
                    ( { model | balances = response }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        ([ Html.text (model.responseMessage |> Maybe.withDefault "")
         , Html.a [ Html.Attributes.href "/account" ] [ Html.text "here" ]
         ]
            ++ List.map (\balance -> Html.div [] [ Html.text balance.item ]) model.balances
        )
