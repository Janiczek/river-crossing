module Problem exposing
    ( Problem
    , ProblemState
    , init
    , moveTo
    )

import AssocList as Dict exposing (Dict)
import Bag
import Entity exposing (Entity)
import Graph exposing (Graph)
import Land exposing (Land)
import Topology exposing (Topology)


type alias Problem =
    { {- Doing this, we're most likely making the topology static. But, given I
         usually lose my concentration and resolve for theproject a few days in
         (and thus have the best chance finishing it if it's a quick
         prototype-like thing), I want to keep the feature creep to the minimum.
      -}
      topology : Topology
    , initial : ProblemState
    , goal : ProblemState
    , current : ProblemState
    }


{-| How does each land look? Is the boat there? What entities are there? etc.
-}
type alias ProblemState =
    Dict Int Land


init :
    { topology : Topology
    , initial : ProblemState
    , goal : ProblemState
    }
    -> Problem
init { topology, initial, goal } =
    { topology = topology
    , initial = initial
    , goal = goal
    , current = initial -- the main gimmick of this function
    }


moveTo : Int -> { landId : Int, entity : Entity } -> Problem -> Problem
moveTo newLandId holded problem =
    {-
       { problem
       | current = problem.current
       |> Dict.update -- remove entity,farmer,boat from old
       |> Dict.update -- add to new, add
       }
    -}
    -- TODO
    problem
