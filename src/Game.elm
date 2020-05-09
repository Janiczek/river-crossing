module Game exposing
    ( InteractionState(..)
    , Item(..)
    , itemFromString
    , itemToId
    , itemToString
    )

import Entity exposing (Entity(..))


type InteractionState
    = DoingNothing
    | HoldingItem
        { landId : Int
        , item : Item
        }


type Item
    = Entity Entity
    | Farmer
    | Land


itemToString : Item -> String
itemToString item =
    case item of
        Entity entity ->
            Entity.toString entity

        Farmer ->
            "Farmer"

        Land ->
            "Land"


itemToId : Item -> String
itemToId item =
    item
        |> itemToString
        |> String.toLower


itemFromString : String -> Maybe Item
itemFromString string =
    case string of
        "wolf" ->
            Just <| Entity Wolf

        "goat" ->
            Just <| Entity Goat

        "cabbage" ->
            Just <| Entity Cabbage

        "land" ->
            Just Land

        "farmer" ->
            Just Farmer

        _ ->
            Nothing
