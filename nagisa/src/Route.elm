module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Top
    | BalanceTable
    | KindElementTable
    | PurposeElementTable
    | PlaceElementTable
    | Login


parse : Url -> Maybe Route
parse url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map BalanceTable (Parser.s "balances")
        , Parser.map KindElementTable (Parser.s "kind" </> Parser.s "elements")
        , Parser.map PurposeElementTable (Parser.s "purpose" </> Parser.s "elements")
        , Parser.map PlaceElementTable (Parser.s "place" </> Parser.s "elements")
        , Parser.map Login (Parser.s "login")
        ]
