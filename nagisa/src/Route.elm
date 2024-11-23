module Route exposing (Route(..), parse, toPath)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | BalanceTable
    | PurposeMoveTable
    | PlaceMoveTable
    | KindElementTable
    | PurposeElementTable
    | PlaceElementTable
    | KindElementCreate
    | PurposeElementCreate
    | PlaceElementCreate
    | KindElementId Int
    | PurposeElementId Int
    | PlaceElementId Int
    | Login


parse : Url -> Maybe Route
parse url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map BalanceTable (Parser.s "balances")
        , Parser.map PurposeMoveTable (Parser.s "purpose" </> Parser.s "moves")
        , Parser.map PlaceMoveTable (Parser.s "place" </> Parser.s "moves")
        , Parser.map KindElementTable (Parser.s "kind" </> Parser.s "elements")
        , Parser.map PurposeElementTable (Parser.s "purpose" </> Parser.s "elements")
        , Parser.map PlaceElementTable (Parser.s "place" </> Parser.s "elements")
        , Parser.map KindElementCreate (Parser.s "kind" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map PurposeElementCreate (Parser.s "purpose" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map PlaceElementCreate (Parser.s "place" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map KindElementId (Parser.s "kind" </> Parser.s "elements" </> Parser.int)
        , Parser.map PurposeElementId (Parser.s "purpose" </> Parser.s "elements" </> Parser.int)
        , Parser.map PlaceElementId (Parser.s "place" </> Parser.s "elements" </> Parser.int)
        , Parser.map Login (Parser.s "login")
        ]


toPath : Route -> String
toPath route =
    case route of
        Top ->
            "/"

        BalanceTable ->
            "/balances"

        PurposeMoveTable ->
            "/purpose/moves"

        PlaceMoveTable ->
            "/place/moves"

        KindElementTable ->
            "/kind/elements"

        PurposeElementTable ->
            "/purpose/elements"

        PlaceElementTable ->
            "/place/elements"

        KindElementCreate ->
            "/kind/elements/create"

        PurposeElementCreate ->
            "/purpose/elements/create"

        PlaceElementCreate ->
            "/place/elements/create"

        KindElementId id ->
            "/kind/elements/" ++ String.fromInt id

        PurposeElementId id ->
            "/purpose/elements/" ++ String.fromInt id

        PlaceElementId id ->
            "/place/elements/" ++ String.fromInt id

        Login ->
            "/login"
