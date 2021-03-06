module Problem exposing
    ( Problem
    , ProblemState
    , hasWon
    , init
    , landHasBoats
    , landHasFarmers
    , moveTo
    , reset
    )

import AssocList as Dict exposing (Dict)
import Bag
import Entity exposing (Entity)
import Game exposing (Item(..))
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


moveTo : Int -> { landId : Int, item : Item } -> Problem -> Problem
moveTo newLandId holded problem =
    let
        entityToMove : Maybe Entity
        entityToMove =
            case holded.item of
                Entity entity ->
                    Just entity

                Farmer ->
                    Nothing

                Game.Land ->
                    Nothing
    in
    { problem
        | current =
            problem.current
                |> Dict.update holded.landId
                    (Maybe.map
                        (\oldLand ->
                            { oldLand
                                | boats = oldLand.boats - 1
                                , farmers = oldLand.boats - 1
                                , entities =
                                    entityToMove
                                        |> Maybe.map (\entity -> Bag.remove entity oldLand.entities)
                                        |> Maybe.withDefault oldLand.entities
                            }
                        )
                    )
                |> Dict.update newLandId
                    (Maybe.map
                        (\newLand ->
                            { newLand
                                | boats = newLand.boats + 1
                                , farmers = newLand.farmers + 1
                                , entities =
                                    entityToMove
                                        |> Maybe.map (\entity -> Bag.insert entity newLand.entities)
                                        |> Maybe.withDefault newLand.entities
                            }
                        )
                    )
    }


reset : Problem -> Problem
reset problem =
    { problem | current = problem.initial }


{-| TODO generalize a bit: don't take the numbers in Goal to be
"exactly this number of items" but "at least this number of items".
-}
hasWon : Problem -> Bool
hasWon { current, goal } =
    eq current goal


eq : ProblemState -> ProblemState -> Bool
eq a b =
    let
        normalize state =
            state
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    List.map2 Tuple.pair
        (normalize a)
        (normalize b)
        |> List.all (\( aLand, bLand ) -> Land.eq aLand bLand)


landHasBoats : Int -> Problem -> Bool
landHasBoats landId problem =
    Dict.get landId problem.current
        |> Maybe.map (\land -> land.boats > 0)
        |> Maybe.withDefault False


landHasFarmers : Int -> Problem -> Bool
landHasFarmers landId problem =
    Dict.get landId problem.current
        |> Maybe.map (\land -> land.farmers > 0)
        |> Maybe.withDefault False
