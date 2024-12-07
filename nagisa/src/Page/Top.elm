module Page.Top exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes as Attributes
import Route


type alias Model =
    {}


type Msg
    = None


init : ( Model, Cmd Msg )
init =
    ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    Html.div []
        [ Html.ul [ Attributes.class "box" ]
            [ getItem Route.Login "ログイン" "image/door-open.svg"
            , getItem Route.BalanceTable "収支表" "image/table.svg"
            , getItem Route.PurposeMoveTable "予算移動" "image/arrow-left-right.svg"
            , getItem Route.PlaceMoveTable "場所移動" "image/arrow-left-right.svg"
            , getItem Route.KindElementTable "種別" "image/layout-sidebar.svg"
            , getItem Route.PurposeElementTable "予算" "image/layout-sidebar.svg"
            , getItem Route.PlaceElementTable "場所" "image/layout-sidebar.svg"
            , getItem Route.Salary "給料入力" "image/cash.svg"
            , getItem Route.Bonus "賞与入力" "image/cash-coin.svg"
            , getItem Route.Monthly "月々の支払入力" "image/calendar3.svg"
            , getItem Route.Secret "秘匿情報" "image/key.svg"
            , getItem Route.Logout "ログアウト" "image/door-closed.svg"
            ]
        ]


getItem : Route.Route -> String -> String -> Html.Html Msg
getItem route text img =
    Html.a
        [ Attributes.href <| Route.toPath route ]
        [ Html.li
            [ Attributes.class "item" ]
            [ Html.text text
            , Html.div [] [ Html.img [ Attributes.src img ] [] ]
            ]
        ]
