module Page.Monthly exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Request.Request as Request
import Route
import String


type alias Model =
    { monthly : StringMonthly
    , xsrfToken : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringMonthly =
    { houseRentAmount : String
    , houseRentDate : String
    , gasAmount : String
    , gasDate : String
    , waterAmount : String
    , waterDate : String
    , electAmount : String
    , electDate : String
    , netAmount : String
    , netDate : String
    }


type Msg
    = InputHouseRentAmount String
    | InputHouseRentDate String
    | InputGasAmount String
    | InputGasDate String
    | InputWaterAmount String
    | InputWaterDate String
    | InputElectAmount String
    | InputElectDate String
    | InputNetAmount String
    | InputNetDate String
    | Insert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        (StringMonthly "" "" "" "" "" "" "" "" "" "")
        xsrfToken
        key
        Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputHouseRentAmount value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | houseRentAmount = value } }, Cmd.none )

        InputHouseRentDate value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | houseRentDate = value } }, Cmd.none )

        InputGasAmount value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | gasAmount = value } }, Cmd.none )

        InputGasDate value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | gasDate = value } }, Cmd.none )

        InputWaterAmount value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | waterAmount = value } }, Cmd.none )

        InputWaterDate value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | waterDate = value } }, Cmd.none )

        InputElectAmount value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | electAmount = value } }, Cmd.none )

        InputElectDate value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | electDate = value } }, Cmd.none )

        InputNetAmount value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | netAmount = value } }, Cmd.none )

        InputNetDate value ->
            let
                newMonthly =
                    model.monthly
            in
            ( { model | monthly = { newMonthly | netDate = value } }, Cmd.none )

        Insert ->
            ( model
            , Request.postMonthly
                model.xsrfToken
                (String.toInt model.monthly.houseRentAmount |> Maybe.withDefault 0)
                model.monthly.houseRentDate
                (String.toInt model.monthly.gasAmount |> Maybe.withDefault 0)
                model.monthly.gasDate
                (String.toInt model.monthly.waterAmount |> Maybe.withDefault 0)
                model.monthly.waterDate
                (String.toInt model.monthly.electAmount |> Maybe.withDefault 0)
                model.monthly.electDate
                (String.toInt model.monthly.netAmount |> Maybe.withDefault 0)
                model.monthly.netDate
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
                [ Html.th [] [ Html.text "家賃" ]
                , Html.th [] [ Html.text "家賃日付" ]
                , Html.th [] [ Html.text "ガス" ]
                , Html.th [] [ Html.text "ガス日付" ]
                , Html.th [] [ Html.text "水道代" ]
                , Html.th [] [ Html.text "水道代日付" ]
                , Html.th [] [ Html.text "電気代" ]
                , Html.th [] [ Html.text "電気代日付" ]
                , Html.th [] [ Html.text "ネット代" ]
                , Html.th [] [ Html.text "ネット日付" ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.monthly.houseRentAmount, onInput InputHouseRentAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.monthly.houseRentDate, onInput InputHouseRentDate ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.monthly.gasAmount, onInput InputGasAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.monthly.gasDate, onInput InputGasDate ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.monthly.waterAmount, onInput InputWaterAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.monthly.waterDate, onInput InputWaterDate ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.monthly.electAmount, onInput InputElectAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.monthly.electDate, onInput InputElectDate ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.monthly.netAmount, onInput InputNetAmount ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.monthly.netDate, onInput InputNetDate ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Insert ] [ Html.text "保存" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
