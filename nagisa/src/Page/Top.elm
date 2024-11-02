module Page.Top exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes
import Http


type alias Model =
    { name : String
    , count : Int
    , domain : String
    }


type Msg
    = None
    | GetJson (Result Http.Error String)


init : String -> Int -> String -> ( Model, Cmd Msg )
init name count domain =
    ( Model name count domain, Http.get { url = domain ++ "/api/balances", expect = Http.expectString GetJson } )


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
        [ Html.text model.domain
        , Html.a [ Html.Attributes.href "/account" ] [ Html.text model.domain ]
        ]
