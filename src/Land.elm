module Land exposing (Land)

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
    , hasBoat : Bool
    , hasFarmer : Bool
    }