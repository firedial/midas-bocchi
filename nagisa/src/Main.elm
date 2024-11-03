module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Page.Account
import Page.BalanceTable
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
    | BalanceTable Page.BalanceTable.Model


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
    | BalanceTableMsg Page.BalanceTable.Msg


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

        TopMsg pageMsg ->
            case model.page of
                Top pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Top.update pageMsg pageModel
                    in
                    ( { model | page = Top newModel }
                    , Cmd.map TopMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        AccountMsg pageMsg ->
            case model.page of
                Account pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Account.update pageMsg pageModel
                    in
                    ( { model | page = Account newModel }
                    , Cmd.map AccountMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        BalanceTableMsg pageMsg ->
            case model.page of
                BalanceTable pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.BalanceTable.update pageMsg pageModel
                    in
                    ( { model | page = BalanceTable newModel }
                    , Cmd.map BalanceTableMsg newCmd
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

            Top pageModel ->
                Page.Top.view pageModel
                    |> Html.map TopMsg

            Account pageModel ->
                Page.Account.view pageModel
                    |> Html.map AccountMsg

            BalanceTable pageModel ->
                Page.BalanceTable.view pageModel
                    |> Html.map BalanceTableMsg
        ]
    }


goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            let
                ( newModel, newCmd ) =
                    Page.Top.init "top" 1
            in
            ( { model | page = Top newModel }
            , Cmd.map TopMsg newCmd
            )

        Just Route.Account ->
            let
                ( newModel, newCmd ) =
                    Page.Account.init "account" 1
            in
            ( { model | page = Account newModel }
            , Cmd.map AccountMsg newCmd
            )

        Just Route.BalanceTable ->
            let
                ( newModel, newCmd ) =
                    Page.BalanceTable.init
            in
            ( { model | page = BalanceTable newModel }
            , Cmd.map BalanceTableMsg newCmd
            )
