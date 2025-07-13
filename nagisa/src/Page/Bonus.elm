module Page.Bonus exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Request.Request as Request
import Route
import String


type alias Model =
    { bonus : StringBonus
    , xsrfToken : String
    , isDisabledEditButton : Bool
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringBonus =
    { bonus : String
    , healthInsurance : String
    , welfarePension : String
    , employmentInsurance : String
    , incomeTax : String
    , date : String
    }


type Msg
    = InputBonus String
    | InputHealthInsurance String
    | InputWelfarePension String
    | InputEmploymentInsurance String
    | InputIncomeTax String
    | InputDate String
    | Insert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        (StringBonus "" "" "" "" "" "")
        xsrfToken
        False
        key
        Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputBonus value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | bonus = value } }, Cmd.none )

        InputHealthInsurance value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | healthInsurance = value } }, Cmd.none )

        InputWelfarePension value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | welfarePension = value } }, Cmd.none )

        InputEmploymentInsurance value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | employmentInsurance = value } }, Cmd.none )

        InputIncomeTax value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | incomeTax = value } }, Cmd.none )

        InputDate value ->
            let
                newBonus =
                    model.bonus
            in
            ( { model | bonus = { newBonus | date = value } }, Cmd.none )

        Insert ->
            ( { model | isDisabledEditButton = True, errorMessage = Nothing }
            , Request.postBonus
                model.xsrfToken
                (String.toInt model.bonus.bonus |> Maybe.withDefault 0)
                (String.toInt model.bonus.healthInsurance |> Maybe.withDefault 0)
                (String.toInt model.bonus.welfarePension |> Maybe.withDefault 0)
                (String.toInt model.bonus.employmentInsurance |> Maybe.withDefault 0)
                (String.toInt model.bonus.incomeTax |> Maybe.withDefault 0)
                model.bonus.date
                ModifiedResult
            )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.Top) )

        ModifiedResult result ->
            case result of
                Ok _ ->
                    ( model, Navigation.pushUrl model.key (Route.toPath Route.Top) )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message, isDisabledEditButton = False }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            [ Html.tr
                []
                [ Html.th [] [ Html.text "賞与" ]
                , Html.th [] [ Html.text "健康保険料" ]
                , Html.th [] [ Html.text "厚生年金" ]
                , Html.th [] [ Html.text "雇用保険料" ]
                , Html.th [] [ Html.text "所得税" ]
                , Html.th [] [ Html.text "日付" ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.bonus.bonus, onInput InputBonus ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.bonus.healthInsurance, onInput InputHealthInsurance ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.bonus.welfarePension, onInput InputWelfarePension ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.bonus.employmentInsurance, onInput InputEmploymentInsurance ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "text", Attributes.value model.bonus.incomeTax, onInput InputIncomeTax ] [] ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.bonus.date, onInput InputDate ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Insert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "保存" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
