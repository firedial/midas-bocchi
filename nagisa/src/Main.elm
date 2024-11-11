module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Page.BalanceTable
import Page.Login
import Page.Top
import Route
import Url


main : Program String Model Msg
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
    | BalanceTable Page.BalanceTable.Model
    | Login Page.Login.Model


type alias Model =
    { key : Navigation.Key
    , page : Page
    , xsrfToken : String
    }


init : String -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken url key =
    Model key NotFound xsrfToken |> goTo (Route.parse url)


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | TopMsg Page.Top.Msg
    | BalanceTableMsg Page.BalanceTable.Msg
    | LoginMsg Page.Login.Msg


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

        LoginMsg pageMsg ->
            case model.page of
                Login pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Login.update pageMsg pageModel
                    in
                    ( { model | page = Login newModel }
                    , Cmd.map LoginMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "midas"
    , body =
        [ Html.header []
            [ Html.h1 [] [ Html.a [ Attributes.href "/" ] [ Html.text "Midas" ] ]
            , Html.nav []
                [ Html.ul []
                    [ Html.li [] [ Html.a [ Attributes.href "/" ] [ Html.text "top" ] ]
                    , Html.li [] [ Html.a [ Attributes.href "/balances" ] [ Html.text "balance" ] ]
                    ]
                ]
            ]
        , case model.page of
            NotFound ->
                Html.text "not found"

            Top pageModel ->
                Page.Top.view pageModel
                    |> Html.map TopMsg

            BalanceTable pageModel ->
                Page.BalanceTable.view pageModel
                    |> Html.map BalanceTableMsg

            Login pageModel ->
                Page.Login.view pageModel
                    |> Html.map LoginMsg
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

        Just Route.BalanceTable ->
            let
                ( newModel, newCmd ) =
                    Page.BalanceTable.init model.xsrfToken
            in
            ( { model | page = BalanceTable newModel }
            , Cmd.map BalanceTableMsg newCmd
            )

        Just Route.Login ->
            let
                ( newModel, newCmd ) =
                    Page.Login.init model.xsrfToken
            in
            ( { model | page = Login newModel }
            , Cmd.map LoginMsg newCmd
            )
