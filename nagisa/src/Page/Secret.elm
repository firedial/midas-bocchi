module Page.Secret exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Model.Enitity.SecretEntity as SecretEntity
import Request.Request as Request
import Route
import String


type alias Model =
    { secret : StringSecret
    , xsrfToken : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringSecret =
    { officeTransportation : String
    , insurance : String
    , houseRent : String
    , net : String
    }


type Msg
    = InputOfficeTransportation String
    | InputInsurance String
    | InputHouseRent String
    | InputNet String
    | GetSecret (Result Request.Error SecretEntity.Secret)
    | Update
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        (StringSecret "" "" "" "")
        xsrfToken
        key
        Nothing
    , Request.getSecret GetSecret
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputOfficeTransportation value ->
            let
                newSecret =
                    model.secret
            in
            ( { model | secret = { newSecret | officeTransportation = value } }, Cmd.none )

        InputInsurance value ->
            let
                newSecret =
                    model.secret
            in
            ( { model | secret = { newSecret | insurance = value } }, Cmd.none )

        InputHouseRent value ->
            let
                newSecret =
                    model.secret
            in
            ( { model | secret = { newSecret | houseRent = value } }, Cmd.none )

        InputNet value ->
            let
                newSecret =
                    model.secret
            in
            ( { model | secret = { newSecret | net = value } }, Cmd.none )

        GetSecret result ->
            case result of
                Ok response ->
                    let
                        stringSecret =
                            StringSecret
                                (String.fromInt response.officeTransportation)
                                (String.fromInt response.insurance)
                                (String.fromInt response.houseRent)
                                (String.fromInt response.net)
                    in
                    ( { model | secret = stringSecret }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Update ->
            ( model
            , Request.putSecret
                model.xsrfToken
                (String.toInt model.secret.officeTransportation |> Maybe.withDefault 0)
                (String.toInt model.secret.insurance |> Maybe.withDefault 0)
                (String.toInt model.secret.houseRent |> Maybe.withDefault 0)
                (String.toInt model.secret.net |> Maybe.withDefault 0)
                ModifiedResult
            )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.Top) )

        ModifiedResult result ->
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
        , Html.table [ Attributes.class "balance" ]
            [ Html.tr
                []
                [ Html.th [] [ Html.text "会社交通費" ]
                , Html.th [] [ Html.text "保険料" ]
                , Html.th [] [ Html.text "家賃" ]
                , Html.th [] [ Html.text "ネット代" ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.secret.officeTransportation, onInput InputOfficeTransportation ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.secret.insurance, onInput InputInsurance ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.secret.houseRent, onInput InputHouseRent ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.secret.net, onInput InputNet ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Update ] [ Html.text "更新" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
