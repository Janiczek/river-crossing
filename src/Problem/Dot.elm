module Problem.Dot exposing (toDot)

{-| DOT is Graphviz textual format
-}

import AssocList as Dict exposing (Dict)
import Bag
import Entity exposing (Entity)
import Game exposing (InteractionState(..), Item(..))
import Graph
import Land exposing (Land)
import Problem exposing (Problem)
import Topology exposing (Topology)


toDot : InteractionState -> Problem -> String
toDot interactionState problem =
    let
        vertices : List Int
        vertices =
            Graph.vertices problem.topology

        edges : List (Graph.Edge Int ())
        edges =
            Graph.edges problem.topology

        lands : List ( Int, Land )
        lands =
            vertices
                |> List.filterMap
                    (\id ->
                        Dict.get id problem.current
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

        toRow : Int -> ( Item, Int ) -> String
        toRow landId ( item, currentCount ) =
            let
                itemId : String
                itemId =
                    Game.itemToId item

                itemLabel : String
                itemLabel =
                    Game.itemToString item

                isActive : Bool
                isActive =
                    case interactionState of
                        DoingNothing ->
                            False

                        HoldingItem h ->
                            h.item == item && h.landId == landId

                isHoldingSomethingElse : Bool
                isHoldingSomethingElse =
                    case interactionState of
                        DoingNothing ->
                            False

                        HoldingItem h ->
                            h.item /= item || h.landId /= landId

                color : String
                color =
                    if
                        not
                            (Problem.landHasBoats landId problem
                                && Problem.landHasFarmers landId problem
                            )
                    then
                        "gray"

                    else if isActive then
                        "blue"

                    else if isHoldingSomethingElse then
                        "gray"

                    else
                        "black"
            in
            "<TR><TD ID="
                ++ quoted (Land.idToString landId ++ "." ++ itemId)
                ++ " PORT="
                ++ quoted itemId
                ++ " HREF="
                ++ quoted " "
                ++ " COLOR="
                ++ quoted color
                ++ "><FONT COLOR="
                ++ quoted color
                ++ ">"
                ++ itemLabel
                ++ ": "
                ++ String.fromInt currentCount
                ++ "x</FONT></TD></TR>\n"

        landTitleRow : Int -> String
        landTitleRow landId =
            let
                color =
                    case interactionState of
                        DoingNothing ->
                            "black"

                        HoldingItem holded ->
                            "red"
            in
            "<TR><TD ID="
                ++ quoted (Land.idToString landId ++ ".land")
                ++ " PORT="
                ++ quoted "land"
                ++ " HREF="
                ++ quoted " "
                ++ " COLOR="
                ++ quoted color
                ++ "><FONT COLOR="
                ++ quoted color
                ++ "><B>Land "
                ++ String.fromInt landId
                ++ "</B></FONT></TD></TR>"

        boatRow : Land -> String
        boatRow land =
            if land.boats > 0 then
                "<TR><TD COLOR="
                    ++ quoted "gray"
                    ++ "><FONT COLOR="
                    ++ quoted "gray"
                    ++ ">Boats: "
                    ++ String.fromInt land.boats
                    ++ "x</FONT></TD></TR>"

            else
                ""

        vertexStrings : List String
        vertexStrings =
            lands
                |> List.map
                    (\( landId, land ) ->
                        let
                            clickables : List ( Item, Int )
                            clickables =
                                (( Farmer, land.farmers )
                                    :: List.map
                                        (Tuple.mapFirst Entity)
                                        (Bag.toCountedList land.entities)
                                )
                                    |> List.filter (\( _, n ) -> n > 0)
                        in
                        Land.idToString landId
                            ++ " [label = < "
                            ++ tableTag
                            ++ landTitleRow landId
                            ++ boatRow land
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
