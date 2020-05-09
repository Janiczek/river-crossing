module Problem exposing
    ( Problem
    , ProblemState
    , init
    , moveTo
    , reset
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
    { problem
        | current =
            problem.current
                |> Dict.update holded.landId
                    (Maybe.map
                        (\oldLand ->
                            { oldLand
                                | -- TODO this should become a decrement of Int
                                  hasBoat = False
                                , hasFarmer = False
                                , entities = Bag.remove holded.entity oldLand.entities
                            }
                        )
                    )
                |> Dict.update newLandId
                    (Maybe.map
                        (\newLand ->
                            { newLand
                                | -- TODO this should become an increment of Int
                                  hasBoat = True
                                , hasFarmer = True
                                , entities = Bag.insert holded.entity newLand.entities
                            }
                        )
                    )
    }


reset : Problem -> Problem
reset problem =
    { problem | current = problem.initial }
