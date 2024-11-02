module Page.Account exposing (Model, Msg, init, update, view)

import Html


type alias Model =
    { name : String
    , count : Int
    }


type Msg
    = Ok
    | Ng


init : String -> Int -> ( Model, Cmd Msg )
init name count =
    ( Model name count, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ok ->
            ( { model | name = "ok" }, Cmd.none )

        Ng ->
            ( { model | name = "ng" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.text model.name
