module Page.Login exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Request.Request as Request


type alias Model =
    { xsrfToken : String
    , errorMessage : Maybe String
    , email : String
    , password : String
    }


type Msg
    = None
    | InputEmail String
    | InputPassword String
    | Login
    | PostLogin (Result Request.Error ())


init : String -> ( Model, Cmd Msg )
init xsrfToken =
    ( Model xsrfToken Nothing "" "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        InputEmail email ->
            ( { model | email = email }, Cmd.none )

        InputPassword password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( model, Request.postLogin model.xsrfToken model.email model.password PostLogin )

        PostLogin result ->
            case result of
                Ok _ ->
                    ( { model | errorMessage = Nothing }, Cmd.none )

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
