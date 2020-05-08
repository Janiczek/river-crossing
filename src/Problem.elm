module Problem exposing
    ( Problem
    , ProblemState
    , init
    , view
    )

import AssocList as Dict exposing (Dict)
import Bag
import Entity
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes as Attrs
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


{-| DOT is Graphviz textual format
-}
toDot : Topology -> ProblemState -> String
toDot topology state =
    let
        vertices : List Int
        vertices =
            Graph.vertices topology

        edges : List (Graph.Edge Int ())
        edges =
            Graph.edges topology

        lands : List ( Int, Land )
        lands =
            vertices
                |> List.filterMap
                    (\id ->
                        Dict.get id state
                            |> Maybe.map (Tuple.pair id)
                    )

        toMaybe : a -> Bool -> Maybe a
        toMaybe value bool =
            if bool then
                Just value

            else
                Nothing

        landId : Int -> String
        landId id =
            "land" ++ String.fromInt id

        vertexStrings : List String
        vertexStrings =
            lands
                |> List.map
                    (\( id, land ) ->
                        let
                            everybody =
                                List.filterMap identity <|
                                    toMaybe "Boat" land.hasBoat
                                        :: toMaybe "Farmer" land.hasFarmer
                                        :: (land.entities
                                                |> Bag.toList
                                                |> List.map (Just << Entity.toString)
                                           )
                        in
                        landId id
                            ++ " [shape=record;label=\""
                            ++ String.join "|" everybody
                            ++ "\";];"
                    )

        edgeStrings : List String
        edgeStrings =
            edges
                |> List.map
                    (\{ from, to } ->
                        landId from ++ " -> " ++ landId to ++ ";"
                    )
    in
    "digraph G {"
        ++ String.join "" vertexStrings
        ++ String.join "" edgeStrings
        ++ "}"


view : Problem -> Html msg
view problem =
    Html.div []
        [ Html.div []
            [ Html.text "Initial"
            , viewGraph problem.topology problem.initial
            ]
        , Html.div []
            [ Html.text "Current"
            , viewGraph problem.topology problem.current
            ]
        , Html.div []
            [ Html.text "Goal"
            , viewGraph problem.topology problem.goal
            ]
        ]


viewGraph : Topology -> ProblemState -> Html msg
viewGraph topology state =
    Html.node "x-viz"
        [ Attrs.attribute "dot" <| toDot topology state ]
        []
