module Request.RequestError exposing (Error(..))


type Error
    = DecodeError String
    | RequestError String
