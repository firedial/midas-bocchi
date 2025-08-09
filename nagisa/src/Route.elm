module Route exposing (Route(..), parse, toPath)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | BalanceTable
    | BalanceCreate
    | BalanceId Int
    | FixedBalance
    | FixedBalanceCreate
    | FixedBalanceId Int
    | PurposeMoveTable
    | PlaceMoveTable
    | PurposeMoveCreate
    | PlaceMoveCreate
    | PurposeMoveId Int
    | PlaceMoveId Int
    | KindElementTable
    | PurposeElementTable
    | PlaceElementTable
    | KindElementCreate
    | PurposeElementCreate
    | PlaceElementCreate
    | KindElementId Int
    | PurposeElementId Int
    | PlaceElementId Int
    | Salary
    | Bonus
    | CheckPlaceSum
    | Secret
    | Login
    | Logout


parse : Url -> Maybe Route
parse url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map BalanceTable (Parser.s "balances")
        , Parser.map BalanceCreate (Parser.s "balances" </> Parser.s "create")
        , Parser.map BalanceId (Parser.s "balances" </> Parser.int)
        , Parser.map FixedBalance (Parser.s "fixed_balances")
        , Parser.map FixedBalanceCreate (Parser.s "fixed_balances" </> Parser.s "create")
        , Parser.map FixedBalanceId (Parser.s "fixed_balances" </> Parser.int)
        , Parser.map PurposeMoveTable (Parser.s "purpose" </> Parser.s "moves")
        , Parser.map PlaceMoveTable (Parser.s "place" </> Parser.s "moves")
        , Parser.map PurposeMoveCreate (Parser.s "purpose" </> Parser.s "moves" </> Parser.s "create")
        , Parser.map PlaceMoveCreate (Parser.s "place" </> Parser.s "moves" </> Parser.s "create")
        , Parser.map PurposeMoveId (Parser.s "purpose" </> Parser.s "moves" </> Parser.int)
        , Parser.map PlaceMoveId (Parser.s "place" </> Parser.s "moves" </> Parser.int)
        , Parser.map KindElementTable (Parser.s "kind" </> Parser.s "elements")
        , Parser.map PurposeElementTable (Parser.s "purpose" </> Parser.s "elements")
        , Parser.map PlaceElementTable (Parser.s "place" </> Parser.s "elements")
        , Parser.map KindElementCreate (Parser.s "kind" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map PurposeElementCreate (Parser.s "purpose" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map PlaceElementCreate (Parser.s "place" </> Parser.s "elements" </> Parser.s "create")
        , Parser.map KindElementId (Parser.s "kind" </> Parser.s "elements" </> Parser.int)
        , Parser.map PurposeElementId (Parser.s "purpose" </> Parser.s "elements" </> Parser.int)
        , Parser.map PlaceElementId (Parser.s "place" </> Parser.s "elements" </> Parser.int)
        , Parser.map Salary (Parser.s "salary")
        , Parser.map Bonus (Parser.s "bonus")
        , Parser.map CheckPlaceSum (Parser.s "check_place_sum")
        , Parser.map Secret (Parser.s "secret")
        , Parser.map Login (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        ]


toPath : Route -> String
toPath route =
    case route of
        Top ->
            "/"

        BalanceTable ->
            "/balances"

        BalanceCreate ->
            "/balances/create"

        BalanceId id ->
            "/balances/" ++ String.fromInt id

        FixedBalance ->
            "/fixed_balances"

        FixedBalanceCreate ->
            "/fixed_balances/create"

        FixedBalanceId id ->
            "/fixed_balances/" ++ String.fromInt id

        PurposeMoveTable ->
            "/purpose/moves"

        PlaceMoveTable ->
            "/place/moves"

        PurposeMoveCreate ->
            "/purpose/moves/create"

        PlaceMoveCreate ->
            "/place/moves/create"

        PurposeMoveId id ->
            "/purpose/moves/" ++ String.fromInt id

        PlaceMoveId id ->
            "/place/moves/" ++ String.fromInt id

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

        Salary ->
            "/salary"

        Bonus ->
            "/bonus"

        CheckPlaceSum ->
            "/check_place_sum"

        Secret ->
            "/secret"

        Login ->
            "/login"

        Logout ->
            "/logout"
