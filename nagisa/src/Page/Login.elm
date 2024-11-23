module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Request.Request as Request
import Route


type alias Model =
    { xsrfToken : String
    , errorMessage : Maybe String
    , key : Navigation.Key
    , email : String
    , password : String
    }


type Msg
    = InputEmail String
    | InputPassword String
    | Login
    | PostLogin (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model xsrfToken Nothing key "" "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email ->
            ( { model | email = email }, Cmd.none )

        InputPassword password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( model, Request.postLogin model.xsrfToken model.email model.password PostLogin )

        PostLogin result ->
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
        , Html.input [ Attributes.type_ "text", Attributes.value model.email, onInput InputEmail ] []
        , Html.input [ Attributes.type_ "password", Attributes.value model.password, onInput InputPassword ] []
        , Html.button [ onClick Login ] [ Html.text "ログイン" ]
        ]
