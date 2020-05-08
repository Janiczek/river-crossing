module Topology exposing
    ( Topology
    , fromEdges
    )

{-| How are the vertices (land masses) connected to each other?

I'd use the Graph data structure directly, but it's directed and I have yet to
create an undirected variant. This is an alternative API that always adds both
directions for an edge.

All the more ironic given I'm the author of the Graph library. TODO make an
undirected variant, damnit!

-}

import Graph exposing (Graph)


type alias Topology =
    Graph Int ()


fromEdges : List ( Int, Int ) -> Topology
fromEdges edges =
    List.foldl
        addUndirectedEdge
        Graph.empty
        edges


{-| In reality, add two edges.
-}
addUndirectedEdge : ( Int, Int ) -> Topology -> Topology
addUndirectedEdge ( a, b ) graph =
    graph
        |> Graph.addEdge a b ()
        |> Graph.addEdge b a ()
