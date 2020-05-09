module Problem.Dot exposing (toDot)

{-| DOT is Graphviz textual format
-}

import AssocList as Dict exposing (Dict)
import Bag
import Entity exposing (Entity)
import Game exposing (InteractionState(..))
import Graph
import Land exposing (Land)
import Problem exposing (ProblemState)
import Topology exposing (Topology)


toDot : InteractionState -> Topology -> ProblemState -> String
toDot interactionState topology state =
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

        toRow : Int -> Entity -> String
        toRow landId entity =
            let
                itemId : String
                itemId =
                    Entity.toId entity

                itemLabel : String
                itemLabel =
                    Entity.toString entity

                isActive : Bool
                isActive =
                    case interactionState of
                        DoingNothing ->
                            False

                        HoldingEntity h ->
                            h.entity == entity && h.landId == landId
            in
            "<TR><TD ID="
                ++ quoted (Land.idToString landId ++ "." ++ itemId)
                ++ " PORT="
                ++ quoted itemId
                ++ " HREF="
                ++ quoted " "
                ++ " COLOR="
                ++ quoted
                    (if isActive then
                        "blue"

                     else
                        "black"
                    )
                ++ "><FONT COLOR="
                ++ quoted
                    (if isActive then
                        "blue"

                     else
                        "black"
                    )
                ++ ">"
                ++ itemLabel
                ++ "</FONT></TD></TR>\n"

        landTitleRow : Int -> String
        landTitleRow landId =
            "<TR><TD ID="
                ++ quoted (Land.idToString landId ++ ".land")
                ++ " PORT="
                ++ quoted "land"
                ++ " HREF="
                ++ quoted " "
                ++ "><B>Land "
                ++ String.fromInt landId
                ++ "</B></TD></TR>"

        boatRow : Land -> String
        boatRow land =
            if land.hasBoat then
                "<TR><TD COLOR=" ++ quoted "gray" ++ "><FONT COLOR=" ++ quoted "gray" ++ ">Boat</FONT></TD></TR>"

            else
                ""

        farmerRow : Land -> String
        farmerRow land =
            if land.hasFarmer then
                "<TR><TD COLOR=" ++ quoted "gray" ++ "><FONT COLOR=" ++ quoted "gray" ++ ">Farmer</FONT></TD></TR>"

            else
                ""

        vertexStrings : List String
        vertexStrings =
            lands
                |> List.map
                    (\( landId, land ) ->
                        let
                            clickables : List Entity
                            clickables =
                                Bag.toList land.entities
                        in
                        Land.idToString landId
                            ++ " [label = < "
                            ++ tableTag
                            ++ landTitleRow landId
                            ++ boatRow land
                            ++ farmerRow land
                            ++ String.join "" (List.map (toRow landId) clickables)
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
