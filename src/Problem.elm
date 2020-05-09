module Problem exposing
    ( Problem
    , ProblemState
    , init
    , toDot
    )

import AssocList as Dict exposing (Dict)
import Bag
import Entity
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
                |> List.sortBy Tuple.first

        toMaybe : a -> Bool -> Maybe a
        toMaybe value bool =
            if bool then
                Just value

            else
                Nothing

        quoted : String -> String
        quoted string =
            "&quot;" ++ string ++ "&quot;"

        tableTag : String
        tableTag =
            "<TABLE BORDER="
                ++ quoted "0"
                ++ " CELLBORDER="
                ++ quoted "1"
                ++ " CELLSPACING="
                ++ quoted "0"
                ++ " CELLPADDING="
                ++ quoted "4"
                ++ ">\n"

        toRow : Int -> ( String, String ) -> String
        toRow id ( itemId, itemLabel ) =
            "<TR><TD ID="
                ++ quoted (Land.idToString id ++ "." ++ itemId)
                ++ " PORT="
                ++ quoted itemId
                ++ " HREF="
                ++ quoted " "
                ++ ">"
                ++ itemLabel
                ++ "</TD></TR>\n"

        landTitleRow : Int -> String
        landTitleRow landId =
            "<TR><TD><B>Land " ++ String.fromInt landId ++ "</B></TD></TR>"

        boatRow : Land -> String
        boatRow land =
            if land.hasBoat then
                "<TR><TD COLOR=" ++ quoted "gray" ++ "><FONT COLOR=" ++ quoted "gray" ++ ">Boat</FONT></TD></TR>"

            else
                ""

        vertexStrings : List String
        vertexStrings =
            lands
                |> List.map
                    (\( id, land ) ->
                        let
                            clickables : List ( String, String )
                            clickables =
                                List.filterMap identity <|
                                    toMaybe ( "farmer", "Farmer" ) land.hasFarmer
                                        :: (land.entities
                                                |> Bag.toList
                                                |> List.map
                                                    (\entity ->
                                                        Just
                                                            ( Entity.toId entity
                                                            , Entity.toString entity
                                                            )
                                                    )
                                           )
                        in
                        Land.idToString id
                            ++ " [label = < "
                            ++ tableTag
                            ++ landTitleRow id
                            ++ boatRow land
                            ++ String.join "" (List.map (toRow id) clickables)
                            ++ "</TABLE> > ];\n"
                    )

        edgeStrings : List String
        edgeStrings =
            edges
                |> List.map
                    (\{ from, to } ->
                        Land.idToString from
                            ++ " -> "
                            ++ Land.idToString to
                            ++ ";\n"
                    )
    in
    "digraph G { node [ shape = plain ];\n"
        ++ String.join "" vertexStrings
        ++ String.join "" edgeStrings
        ++ "}"
