module Page.TemplateTable exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import List
import Maybe
import Model.Enitity.TemplateEntity as TemplateEntity
import Request.Request as Request
import Route
import String


type alias Model =
    { templates : TemplateEntity.Templates
    , errorMessage : Maybe String
    }


type Msg
    = GetTemplates (Result Request.Error TemplateEntity.Templates)


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing
    , Request.getTemplates GetTemplates
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTemplates result ->
            case result of
                Ok response ->
                    ( { model | templates = response }, Cmd.none )

                Err (Request.DecodeError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )

                Err (Request.RequestError message) ->
                    ( { model | errorMessage = Just message }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text (model.errorMessage |> Maybe.withDefault "")
        , Html.table [ Attributes.class "balance" ]
            (Html.tr []
                [ Html.th [] [ Html.text "id" ]
                , Html.th [] [ Html.text "名前" ]
                ]
                :: Html.tr []
                    [ Html.td [] [ Html.a [ Attributes.href (Route.toPath Route.TemplateCreate) ] [ Html.text "+" ] ]
                    , Html.td [] [ Html.text "" ]
                    ]
                :: List.map
                    (\template ->
                        Html.tr []
                            [ Html.td [] [ Html.a [ Attributes.href (Route.toPath (Route.TemplateId template.id)) ] [ Html.text <| String.fromInt template.id ] ]
                            , Html.td [] [ Html.text template.name ]
                            ]
                    )
                    model.templates
            )
        ]
