module Model.Enitity.MoveEntity exposing (Move, Moves)


type alias Move =
    { id : Int
    , amount : Int
    , item : String
    , beforeId : Int
    , afterId : Int
    , date : String
    , beforeDescription : String
    , afterDescription : String
    }


type alias Moves =
    List Move
