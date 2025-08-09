module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Model.ValueObject.MoveAttributeValueObject as MoveAttributeValueObject
import Page.BalanceId
import Page.BalanceTable
import Page.Bonus
import Page.CheckPlaceSum
import Page.ElementId
import Page.ElementTable
import Page.FixedBalance
import Page.FixedBalanceId
import Page.Login
import Page.Logout
import Page.Monthly
import Page.MoveId
import Page.MoveTable
import Page.Salary
import Page.Secret
import Page.Top
import Page.Transportation
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
    | BalanceId Page.BalanceId.Model
    | FixedBalance Page.FixedBalance.Model
    | FixedBalanceId Page.FixedBalanceId.Model
    | MoveTable Page.MoveTable.Model
    | MoveId Page.MoveId.Model
    | ElementTable Page.ElementTable.Model
    | ElementId Page.ElementId.Model
    | Salary Page.Salary.Model
    | Bonus Page.Bonus.Model
    | Monthly Page.Monthly.Model
    | Transportation Page.Transportation.Model
    | CheckPlaceSum Page.CheckPlaceSum.Model
    | Secret Page.Secret.Model
    | Login Page.Login.Model
    | Logout Page.Logout.Model


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
    | BalanceIdMsg Page.BalanceId.Msg
    | FixedBalanceMsg Page.FixedBalance.Msg
    | FixedBalanceIdMsg Page.FixedBalanceId.Msg
    | MoveTableMsg Page.MoveTable.Msg
    | MoveIdMsg Page.MoveId.Msg
    | ElementTableMsg Page.ElementTable.Msg
    | ElementIdMsg Page.ElementId.Msg
    | SalaryMsg Page.Salary.Msg
    | BonusMsg Page.Bonus.Msg
    | MonthlyMsg Page.Monthly.Msg
    | TransportationMsg Page.Transportation.Msg
    | CheckPlaceSumMsg Page.CheckPlaceSum.Msg
    | SecretMsg Page.Secret.Msg
    | LoginMsg Page.Login.Msg
    | LogoutMsg Page.Logout.Msg


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

        BalanceIdMsg pageMsg ->
            case model.page of
                BalanceId pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.BalanceId.update pageMsg pageModel
                    in
                    ( { model | page = BalanceId newModel }
                    , Cmd.map BalanceIdMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        FixedBalanceMsg pageMsg ->
            case model.page of
                FixedBalance pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.FixedBalance.update pageMsg pageModel
                    in
                    ( { model | page = FixedBalance newModel }
                    , Cmd.map FixedBalanceMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        FixedBalanceIdMsg pageMsg ->
            case model.page of
                FixedBalanceId pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.FixedBalanceId.update pageMsg pageModel
                    in
                    ( { model | page = FixedBalanceId newModel }
                    , Cmd.map FixedBalanceIdMsg newCmd
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

        SalaryMsg pageMsg ->
            case model.page of
                Salary pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Salary.update pageMsg pageModel
                    in
                    ( { model | page = Salary newModel }
                    , Cmd.map SalaryMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        BonusMsg pageMsg ->
            case model.page of
                Bonus pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Bonus.update pageMsg pageModel
                    in
                    ( { model | page = Bonus newModel }
                    , Cmd.map BonusMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        MonthlyMsg pageMsg ->
            case model.page of
                Monthly pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Monthly.update pageMsg pageModel
                    in
                    ( { model | page = Monthly newModel }
                    , Cmd.map MonthlyMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TransportationMsg pageMsg ->
            case model.page of
                Transportation pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Transportation.update pageMsg pageModel
                    in
                    ( { model | page = Transportation newModel }
                    , Cmd.map TransportationMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        CheckPlaceSumMsg pageMsg ->
            case model.page of
                CheckPlaceSum pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.CheckPlaceSum.update pageMsg pageModel
                    in
                    ( { model | page = CheckPlaceSum newModel }
                    , Cmd.map CheckPlaceSumMsg newCmd
                    )

                _ ->
                    ( model, Cmd.none )

        SecretMsg pageMsg ->
            case model.page of
                Secret pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Secret.update pageMsg pageModel
                    in
                    ( { model | page = Secret newModel }
                    , Cmd.map SecretMsg newCmd
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

        LogoutMsg pageMsg ->
            case model.page of
                Logout pageModel ->
                    let
                        ( newModel, newCmd ) =
                            Page.Logout.update pageMsg pageModel
                    in
                    ( { model | page = Logout newModel }
                    , Cmd.map LogoutMsg newCmd
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
                    , Html.li [] [ Html.a [ Attributes.href (Route.toPath Route.BalanceCreate) ] [ Html.text "balance_create" ] ]
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

            BalanceId pageModel ->
                Page.BalanceId.view pageModel
                    |> Html.map BalanceIdMsg

            FixedBalance pageModel ->
                Page.FixedBalance.view pageModel
                    |> Html.map FixedBalanceMsg

            FixedBalanceId pageModel ->
                Page.FixedBalanceId.view pageModel
                    |> Html.map FixedBalanceIdMsg

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

            Salary pageModel ->
                Page.Salary.view pageModel
                    |> Html.map SalaryMsg

            Bonus pageModel ->
                Page.Bonus.view pageModel
                    |> Html.map BonusMsg

            Monthly pageModel ->
                Page.Monthly.view pageModel
                    |> Html.map MonthlyMsg

            Transportation pageModel ->
                Page.Transportation.view pageModel
                    |> Html.map TransportationMsg

            CheckPlaceSum pageModel ->
                Page.CheckPlaceSum.view pageModel
                    |> Html.map CheckPlaceSumMsg

            Secret pageModel ->
                Page.Secret.view pageModel
                    |> Html.map SecretMsg

            Login pageModel ->
                Page.Login.view pageModel
                    |> Html.map LoginMsg

            Logout pageModel ->
                Page.Logout.view pageModel
                    |> Html.map LogoutMsg
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
                    Page.Top.init
            in
            ( { model | page = Top newModel }
            , Cmd.map TopMsg newCmd
            )

        Just Route.BalanceTable ->
            let
                ( newModel, newCmd ) =
                    Page.BalanceTable.init
            in
            ( { model | page = BalanceTable newModel }
            , Cmd.map BalanceTableMsg newCmd
            )

        Just Route.BalanceCreate ->
            let
                ( newModel, newCmd ) =
                    Page.BalanceId.init model.xsrfToken model.key Nothing
            in
            ( { model | page = BalanceId newModel }
            , Cmd.map BalanceIdMsg newCmd
            )

        Just (Route.BalanceId id) ->
            let
                ( newModel, newCmd ) =
                    Page.BalanceId.init model.xsrfToken model.key (Just id)
            in
            ( { model | page = BalanceId newModel }
            , Cmd.map BalanceIdMsg newCmd
            )

        Just Route.FixedBalance ->
            let
                ( newModel, newCmd ) =
                    Page.FixedBalance.init
            in
            ( { model | page = FixedBalance newModel }
            , Cmd.map FixedBalanceMsg newCmd
            )

        Just Route.FixedBalanceCreate ->
            let
                ( newModel, newCmd ) =
                    Page.FixedBalanceId.init model.xsrfToken model.key Nothing
            in
            ( { model | page = FixedBalanceId newModel }
            , Cmd.map FixedBalanceIdMsg newCmd
            )

        Just (Route.FixedBalanceId id) ->
            let
                ( newModel, newCmd ) =
                    Page.FixedBalanceId.init model.xsrfToken model.key (Just id)
            in
            ( { model | page = FixedBalanceId newModel }
            , Cmd.map FixedBalanceIdMsg newCmd
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
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Place Nothing
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
                    Page.MoveId.init model.xsrfToken model.key MoveAttributeValueObject.Place (Just id)
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

        Just Route.Salary ->
            let
                ( newModel, newCmd ) =
                    Page.Salary.init model.xsrfToken model.key
            in
            ( { model | page = Salary newModel }
            , Cmd.map SalaryMsg newCmd
            )

        Just Route.Bonus ->
            let
                ( newModel, newCmd ) =
                    Page.Bonus.init model.xsrfToken model.key
            in
            ( { model | page = Bonus newModel }
            , Cmd.map BonusMsg newCmd
            )

        Just Route.Monthly ->
            let
                ( newModel, newCmd ) =
                    Page.Monthly.init model.xsrfToken model.key
            in
            ( { model | page = Monthly newModel }
            , Cmd.map MonthlyMsg newCmd
            )

        Just Route.Transportation ->
            let
                ( newModel, newCmd ) =
                    Page.Transportation.init model.xsrfToken model.key
            in
            ( { model | page = Transportation newModel }
            , Cmd.map TransportationMsg newCmd
            )

        Just Route.CheckPlaceSum ->
            let
                ( newModel, newCmd ) =
                    Page.CheckPlaceSum.init model.xsrfToken model.key
            in
            ( { model | page = CheckPlaceSum newModel }
            , Cmd.map CheckPlaceSumMsg newCmd
            )

        Just Route.Secret ->
            let
                ( newModel, newCmd ) =
                    Page.Secret.init model.xsrfToken model.key
            in
            ( { model | page = Secret newModel }
            , Cmd.map SecretMsg newCmd
            )

        Just Route.Login ->
            let
                ( newModel, newCmd ) =
                    Page.Login.init model.xsrfToken model.key
            in
            ( { model | page = Login newModel }
            , Cmd.map LoginMsg newCmd
            )

        Just Route.Logout ->
            let
                ( newModel, newCmd ) =
                    Page.Logout.init model.xsrfToken model.key
            in
            ( { model | page = Logout newModel }
            , Cmd.map LogoutMsg newCmd
            )
