module Page.CheckPlaceSum exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput)
import Maybe
import Model.Enitity.AttributeElementEntity as AttributeElementEntity
import Model.ValueObject.AttributeValueObject as AttributeValueObject
import Request.Request as Request
import Route


type alias Model =
    { checkPlaeSum : StringCheckPlaceSum
    , attributeElements : AttributeElementEntity.AttributeElements
    , isDisabledEditButton : Bool
    , xsrfToken : String
    , key : Navigation.Key
    , errorMessage : Maybe String
    }


type alias StringCheckPlaceSum =
    { sum : String
    , placeElementId : String
    , date : String
    }


type Msg
    = InputPlaceElementId String
    | InputDate String
    | GetAttributeElements (Result Request.Error AttributeElementEntity.AttributeElements)
    | Insert
    | Cancel
    | ModifiedResult (Result Request.Error ())


init : String -> Navigation.Key -> ( Model, Cmd Msg )
init xsrfToken key =
    ( Model
        (StringCheckPlaceSum "" "" "")
        []
        False
        xsrfToken
        key
        Nothing
    , Request.getAttributeElements AttributeValueObject.Place GetAttributeElements
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputPlaceElementId placeElementId ->
            let
                newCheckPlaceSum =
                    model.checkPlaeSum
            in
            ( { model | checkPlaeSum = { newCheckPlaceSum | placeElementId = placeElementId } }, Cmd.none )

        InputDate date ->
            let
                newCheckPlaceSum =
                    model.checkPlaeSum
            in
            ( { model | checkPlaeSum = { newCheckPlaceSum | date = date } }, Cmd.none )

        GetAttributeElements result ->
            case result of
                Ok response ->
                    ( { model | attributeElements = response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

        Insert ->
            ( { model | isDisabledEditButton = True, errorMessage = Nothing }
            , Request.postCheckPlaceSum
                model.xsrfToken
                (String.toInt model.checkPlaeSum.sum |> Maybe.withDefault 0)
                (String.toInt model.checkPlaeSum.placeElementId |> Maybe.withDefault 0)
                model.checkPlaeSum.date
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
                [ Html.th [] [ Html.text "場所ID" ]
                , Html.th [] [ Html.text "日付" ]
                ]
            , Html.tr []
                [ Html.td []
                    [ Html.select [ onInput InputPlaceElementId, Attributes.value model.checkPlaeSum.placeElementId ]
                        (List.map
                            (\attributeElement ->
                                Html.option
                                    [ Attributes.value <| String.fromInt attributeElement.id
                                    , Attributes.selected (String.fromInt attributeElement.id == model.checkPlaeSum.placeElementId)
                                    ]
                                    [ Html.text <| attributeElement.description ]
                            )
                            model.attributeElements
                        )
                    ]
                , Html.td [] [ Html.input [ Attributes.type_ "date", Attributes.value model.checkPlaeSum.date, onInput InputDate ] [] ]
                ]
            , Html.div []
                [ Html.button [ Attributes.class "edit-button", onClick Insert, Attributes.disabled model.isDisabledEditButton ] [ Html.text "保存" ] ]
            , Html.div []
                [ Html.button [ Attributes.class "cancel-button", onClick Cancel ] [ Html.text "キャンセル" ] ]
            ]
        ]
