module Page.Top exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes
import Http


type alias Model =
    { name : String
    , count : Int
    }


type Msg
    = GetJson (Result Http.Error String)


init : String -> Int -> ( Model, Cmd Msg )
init name count =
    ( Model name count, Http.get { url = "/api/balances", expect = Http.expectString GetJson } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
        , Html.a [ Html.Attributes.href "/balances" ] [ Html.text "balance" ]
        ]