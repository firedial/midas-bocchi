module Page.Salary exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Request.Request as Request
import Route
import String


type alias Model =
    { salary : StringSalary
    , xsrfToken : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringSalary =
    { baseSalary : String
    , adjustmentSalary : String
    , transportation : String
    , holdingIncentives : String
    , healthInsurance : String
    , welfarePension : String
    , residentTax : String
    , employmentInsurance : String
    , incomeTax : String
    , holding : String
    , date : String
    }


type Msg
    = InputBaseSalary String
    | InputAdjustmentSalary String
    | InputTrasportation String
    | InputHoldingIncentives String
    | InputHealthInsurance String
    | InputWelfarePension String
    | InputResidentTax String
    | InputEmploymentInsurance String
    | InputIncomeTax String
    | InputHolding String
    | InputDate String
    | Insert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        (StringSalary "" "" "" "" "" "" "" "" "" "" "")
        xsrfToken
        key
        Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputBaseSalary value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | baseSalary = value } }, Cmd.none )

        InputAdjustmentSalary value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | adjustmentSalary = value } }, Cmd.none )

        InputTrasportation value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | transportation = value } }, Cmd.none )

        InputHoldingIncentives value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | holdingIncentives = value } }, Cmd.none )

        InputHealthInsurance value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | healthInsurance = value } }, Cmd.none )

        InputWelfarePension value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | welfarePension = value } }, Cmd.none )

        InputResidentTax value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | residentTax = value } }, Cmd.none )

        InputEmploymentInsurance value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | employmentInsurance = value } }, Cmd.none )

        InputIncomeTax value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | incomeTax = value } }, Cmd.none )

        InputHolding value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | holding = value } }, Cmd.none )

        InputDate value ->
            let
                newSalary =
                    model.salary
            in
            ( { model | salary = { newSalary | date = value } }, Cmd.none )

        Insert ->
            ( model
            , Request.postSalary
                model.xsrfToken
                (String.toInt model.salary.baseSalary |> Maybe.withDefault 0)
                (String.toInt model.salary.adjustmentSalary |> Maybe.withDefault 0)
                (String.toInt model.salary.transportation |> Maybe.withDefault 0)
                (String.toInt model.salary.holdingIncentives |> Maybe.withDefault 0)
                (String.toInt model.salary.healthInsurance |> Maybe.withDefault 0)
                (String.toInt model.salary.welfarePension |> Maybe.withDefault 0)
                (String.toInt model.salary.residentTax |> Maybe.withDefault 0)
                (String.toInt model.salary.employmentInsurance |> Maybe.withDefault 0)
                (String.toInt model.salary.incomeTax |> Maybe.withDefault 0)
                (String.toInt model.salary.holding |> Maybe.withDefault 0)
                model.salary.date
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
                [ Html.th [] [ Html.text "基本給" ]
                , Html.th [] [ Html.text "職務調整給" ]
                , Html.th [] [ Html.text "交通費" ]
                , Html.th [] [ Html.text "持株奨励金" ]
                , Html.th [] [ Html.text "健康保険料" ]
                , Html.th [] [ Html.text "厚生年金" ]
                , Html.th [] [ Html.text "住民税" ]
                , Html.th [] [ Html.text "雇用保険料" ]
                , Html.th [] [ Html.text "所得税" ]
                , Html.th [] [ Html.text "持株" ]
                , Html.th [] [ Html.text "日付" ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.baseSalary, onInput InputBaseSalary ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.adjustmentSalary, onInput InputAdjustmentSalary ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.transportation, onInput InputTrasportation ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.holdingIncentives, onInput InputHoldingIncentives ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.healthInsurance, onInput InputHealthInsurance ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.welfarePension, onInput InputWelfarePension ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.residentTax, onInput InputResidentTax ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.employmentInsurance, onInput InputEmploymentInsurance ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.incomeTax, onInput InputIncomeTax ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.salary.holding, onInput InputHolding ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.salary.date, onInput InputDate ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Insert ] [ Html.text "保存" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
