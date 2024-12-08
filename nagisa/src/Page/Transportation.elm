module Page.Transportation exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Request.Request as Request
import Route
import String


type alias Model =
    { date : String
    , isDisabledEditButton : Bool
    , xsrfToken : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type Msg
    = InputDate String
    | Insert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        ""
        False
        xsrfToken
        key
        Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputDate value ->
            ( { model | date = value }, Cmd.none )

        Insert ->
            ( { model | isDisabledEditButton = True }
            , Request.postTransportation
                model.xsrfToken
                model.date
                ModifiedResult
            )

        Cancel ->
            ( model, Navigation.pushUrl model.key (Route.toPath Route.Top) )

        ModifiedResult result ->
            case result of
                Ok _ ->
                    ( { model | errorMessage = Just "OK", isDisabledEditButton = False }, Cmd.none )

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
                [ Html.th [] [ Html.text "日付" ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.date, onInput InputDate ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Insert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "保存" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
