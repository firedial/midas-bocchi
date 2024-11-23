module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Page.BalanceTable
import Page.ElementId
import Page.ElementTable
import Page.Login
import Page.MoveId
import Page.MoveTable
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
    | MoveTable Page.MoveTable.Model
    | MoveId Page.MoveId.Model
    | ElementTable Page.ElementTable.Model
    | ElementId Page.ElementId.Model
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
    | MoveTableMsg Page.MoveTable.Msg
    | MoveIdMsg Page.MoveId.Msg
    | ElementTableMsg Page.ElementTable.Msg
    | ElementIdMsg Page.ElementId.Msg
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

        ElementTableMsg pageMsg ->
            case model.page of
                ElementTable pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.ElementTable.update pageMsg pageModel
                    in
                    ( { model | page = ElementTable newModel }
                    , Cmd.map ElementTableMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        MoveTableMsg pageMsg ->
            case model.page of
                MoveTable pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.MoveTable.update pageMsg pageModel
                    in
                    ( { model | page = MoveTable newModel }
                    , Cmd.map MoveTableMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        MoveIdMsg pageMsg ->
            case model.page of
                MoveId pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.MoveId.update pageMsg pageModel
                    in
                    ( { model | page = MoveId newModel }
                    , Cmd.map MoveIdMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ElementIdMsg pageMsg ->
            case model.page of
                ElementId pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.ElementId.update pageMsg pageModel
                    in
                    ( { model | page = ElementId newModel }
                    , Cmd.map ElementIdMsg newCmd
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
            [ Html.h1 [] [ Html.a [ Attributes.href (Route.toPath Route.Top) ] [ Html.text "Midas" ] ]
            , Html.nav []
                [ Html.ul []
                    [ Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.Top) ] [ Html.text "top" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.BalanceTable) ] [ Html.text "balance" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.KindElementTable) ] [ Html.text "kind_element" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.PurposeElementTable) ] [ Html.text "purpose_element" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.PlaceElementTable) ] [ Html.text "place_element" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.PurposeMoveTable) ] [ Html.text "purpose_move" ] ]
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.PlaceMoveTable) ] [ Html.text "place_move" ] ]
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

            MoveTable pageModel ->
                Page.MoveTable.view pageModel
                    |> Html.map MoveTableMsg

            MoveId pageModel ->
                Page.MoveId.view pageModel
                    |> Html.map MoveIdMsg

            ElementTable pageModel ->
                Page.ElementTable.view pageModel
                    |> Html.map ElementTableMsg

            ElementId pageModel ->
                Page.ElementId.view pageModel
                    |> Html.map ElementIdMsg

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

        Just Route.PurposeMoveTable ->
            let
                ( newModel, newCmd ) =
                    Page.MoveTable.init MoveAttributeValueObject.Purpose
            in
            ( { model | page = MoveTable newModel }
            , Cmd.map MoveTableMsg newCmd
            )

        Just Route.PlaceMoveTable ->
            let
                ( newModel, newCmd ) =
                    Page.MoveTable.init MoveAttributeValueObject.Place
            in
            ( { model | page = MoveTable newModel }
            , Cmd.map MoveTableMsg newCmd
            )

        Just Route.PurposeMoveCreate ->
            let
                ( newModel, newCmd ) =
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Purpose Nothing
            in
            ( { model | page = MoveId newModel }
            , Cmd.map MoveIdMsg newCmd
            )

        Just Route.PlaceMoveCreate ->
            let
                ( newModel, newCmd ) =
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Purpose Nothing
            in
            ( { model | page = MoveId newModel }
            , Cmd.map MoveIdMsg newCmd
            )

        Just (Route.PurposeMoveId id) ->
            let
                ( newModel, newCmd ) =
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Purpose (Just id)
            in
            ( { model | page = MoveId newModel }
            , Cmd.map MoveIdMsg newCmd
            )

        Just (Route.PlaceMoveId id) ->
            let
                ( newModel, newCmd ) =
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Purpose (Just id)
            in
            ( { model | page = MoveId newModel }
            , Cmd.map MoveIdMsg newCmd
            )

        Just Route.KindElementTable ->
            let
                ( newModel, newCmd ) =
                    Page.ElementTable.init AttributeValueObject.Kind
            in
            ( { model | page = ElementTable newModel }
            , Cmd.map ElementTableMsg newCmd
            )

        Just Route.PurposeElementTable ->
            let
                ( newModel, newCmd ) =
                    Page.ElementTable.init AttributeValueObject.Purpose
            in
            ( { model | page = ElementTable newModel }
            , Cmd.map ElementTableMsg newCmd
            )

        Just Route.PlaceElementTable ->
            let
                ( newModel, newCmd ) =
                    Page.ElementTable.init AttributeValueObject.Place
            in
            ( { model | page = ElementTable newModel }
            , Cmd.map ElementTableMsg newCmd
            )

        Just Route.KindElementCreate ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Kind Nothing
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just Route.PurposeElementCreate ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Purpose Nothing
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just Route.PlaceElementCreate ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Place Nothing
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just (Route.KindElementId id) ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Kind (Just id)
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just (Route.PurposeElementId id) ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Purpose (Just id)
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just (Route.PlaceElementId id) ->
            let
                ( newModel, newCmd ) =
                    Page.ElementId.init model.xsrfToken model.key AttributeValueObject.Place (Just id)
            in
            ( { model | page = ElementId newModel }
            , Cmd.map ElementIdMsg newCmd
            )

        Just Route.Login ->
            let
                ( newModel, newCmd ) =
                    Page.Login.init model.xsrfToken
            in
            ( { model | page = Login newModel }
            , Cmd.map LoginMsg newCmd
            )
