module Page.BalanceTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes
import Http


type alias Model =
    { name : String
    , count : Int
    }


type Msg
    = None
    | GetJson (Result Http.Error String)


init : String -> Int -> ( Model, Cmd Msg )
init name count =
    ( Model name count, Http.get { url = "/api/balances", expect = Http.expectString GetJson } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( { model | name = "top none" }, Cmd.none )

        GetJson result ->
            case result of
                Ok response ->
                    ( { model | name = response }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text model.name
        , Html.a [ Html.Attributes.href "/account" ] [ Html.text "here" ]
        ]
