module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Top
    | Account


parse : Url -> Maybe Route
parse url =
    Parser.parse routes url


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map Account (Parser.s "account")
        , Parser.map Top (Parser.s "index.html")
        ]
