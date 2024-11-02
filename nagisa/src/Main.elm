module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Page.Account
import Page.Top
import Route
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


type Page
    = NotFound
    | Top Page.Top.Model
    | Account Page.Account.Model


type alias Model =
    { key : Navigation.Key
    , page : Page
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    Model key NotFound |> goTo (Route.parse url)


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | TopMsg Page.Top.Msg
    | AccountMsg Page.Account.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            goTo (Route.parse url) model

        TopMsg topMsg ->
            case model.page of
                Top topModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Top.update topMsg topModel
                    in
                    ( { model | page = Top newModel }
                    , Cmd.map TopMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        AccountMsg accountMsg ->
            case model.page of
                Account accountModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Account.update accountMsg accountModel
                    in
                    ( { model | page = Account newModel }
                    , Cmd.map AccountMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "midas"
    , body =
        [ Html.br [] []
        , case model.page of
            NotFound ->
                Html.text "not found"

            Top topModel ->
                Page.Top.view topModel
                    |> Html.map TopMsg

            Account accountModel ->
                Page.Account.view accountModel
                    |> Html.map AccountMsg
        ]
    }


goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            let
                ( topModel, topCmd ) =
                    Page.Top.init "top" 1
            in
            ( { model | page = Top topModel }
            , Cmd.map TopMsg topCmd
            )

        Just Route.Account ->
            let
                ( accountModel, accountCmd ) =
                    Page.Account.init "account" 1
            in
            ( { model | page = Account accountModel }
            , Cmd.map AccountMsg accountCmd
            )
