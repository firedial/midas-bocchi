module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Top
    | BalanceTable
    | Login


parse : Url -> Maybe Route
parse url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map BalanceTable (Parser.s "balances")
        , Parser.map Login (Parser.s "login")
        ]
