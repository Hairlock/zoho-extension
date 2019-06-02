module Model exposing (Model)

-- This is the model in common among all of our apps


type alias Model =
    { clicks : Int
    , toggled : Bool
    }
