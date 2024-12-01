module Page.Logout exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Events exposing (onClick)
import Request.Request as Request
import Route


type alias Model =
    { xsrfToken : String
    , errorMessage : Maybe String
    , key : Navigation.Key
    }


type Msg
    = Logout
    | PostLogout (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model xsrfToken Nothing key, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Logout ->
            ( model, Request.postLogout model.xsrfToken PostLogout )

        PostLogout result ->
            case result of
                Ok _ ->
                    ( model, Navigation.pushUrl model.key (Route.toPath Route.Top) )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.button [ onClick Logout ] [ Html.text "ログアウト" ]
        ]
