module Land exposing (Land, eq, idFromString, idToString)

{-| The vertex in our problem graph

A land mass (a bank of the river) which has some entities and possibly a boat
and/or the farmer on it.

-}

import Bag exposing (Bag)
import Entity exposing (Entity)


type alias Land =
    { {- By using a Bag instead of Set, we're giving
         ourselves the possibility to have multiple of each entity at once!
      -}
      entities : Bag Entity
    , boats : Int
    , farmers : Int
    }


idToString : Int -> String
idToString n =
    "land" ++ String.fromInt n


idFromString : String -> Maybe Int
idFromString string =
    case String.split "land" string of
        [ "", idString ] ->
            String.toInt idString

        _ ->
            Nothing


eq : Land -> Land -> Bool
eq a b =
    (a.boats == b.boats)
        && (a.farmers == b.farmers)
        && Bag.eq a.entities b.entities
