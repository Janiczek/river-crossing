module ForceLayout exposing (LayoutSettings, findPositions)

{-| Taken and adapted from [jhrcek/force-layout](https://github.com/jhrcek/force-layout).
Thanks, Honza!

Also, while reworking this I've made a huge mess. Condolences to anybody who
tries to understand it!

-}

import AssocList as Dict exposing (Dict)
import Graph exposing (Graph)
import Math.Vector2 as V2 exposing (Vec2)


type alias LayoutSettings =
    { charge : Float
    , stiffness : Float
    , speed : Float
    }


type alias LayoutGraph =
    { graph : Graph Int ()
    , positions : Dict Int Vec2
    }


findPositions : LayoutSettings -> Graph vertex edge -> List ( vertex, Vec2 )
findPositions layoutSettings graph =
    let
        vertices : List vertex
        vertices =
            Graph.vertices graph

        vertexForId : Dict Int vertex
        vertexForId =
            Graph.vertices graph
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        idForVertex : Dict vertex Int
        idForVertex =
            Graph.vertices graph
                |> List.indexedMap (\id vertex -> ( vertex, id ))
                |> Dict.fromList

        layoutGraph : LayoutGraph
        layoutGraph =
            { graph = Graph.fromVerticesAndEdges [] edgesWithIds
            , positions =
                vertexForId
                    |> Dict.map
                        (\id _ ->
                            let
                                floatId =
                                    toFloat id
                            in
                            V2.vec2 floatId floatId
                        )
            }

        edgesWithIds : List (Graph.Edge Int ())
        edgesWithIds =
            Graph.edges graph
                |> List.filterMap
                    (\edge ->
                        Maybe.map2
                            (\fromId toId ->
                                { from = fromId
                                , to = toId
                                , data = ()
                                }
                            )
                            (Dict.get edge.from idForVertex)
                            (Dict.get edge.to idForVertex)
                    )

        finalLayoutGraph : LayoutGraph
        finalLayoutGraph =
            iterate
                {- The positions are good enough if sum of distances of the points
                   didn't change _much_ in this iteration.
                -}
                (\old new ->
                    (List.sum <|
                        List.map2 V2.distance
                            (Dict.values old.positions)
                            (Dict.values new.positions)
                    )
                        < 1
                )
                (updatePositions layoutSettings)
                layoutGraph
    in
    finalLayoutGraph.graph
        |> Graph.vertices
        |> List.filterMap
            (\id ->
                Maybe.map2 Tuple.pair
                    (Dict.get id vertexForId)
                    (Dict.get id finalLayoutGraph.positions)
            )


iterate : (a -> a -> Bool) -> (a -> a) -> a -> a
iterate isGoodEnough fn value =
    iterateHelp 100 0 isGoodEnough fn value


iterateHelp : Int -> Int -> (a -> a -> Bool) -> (a -> a) -> a -> a
iterateHelp maxI i isGoodEnough fn value =
    let
        newValue : a
        newValue =
            fn value
    in
    if i >= maxI || isGoodEnough value newValue then
        value

    else
        iterateHelp maxI (i + 1) isGoodEnough fn newValue


updatePositions : LayoutSettings -> LayoutGraph -> LayoutGraph
updatePositions settings layoutGraph =
    Graph.fold
        (\vertexId accLayoutGraph ->
            let
                newPosition =
                    updatePosition settings accLayoutGraph vertexId
            in
            { accLayoutGraph
                | positions = Dict.insert vertexId newPosition accLayoutGraph.positions
            }
        )
        layoutGraph
        layoutGraph.graph


updatePosition : LayoutSettings -> LayoutGraph -> Int -> Vec2
updatePosition { charge, stiffness, speed } { graph, positions } vertexId =
    let
        vertexPosition =
            Dict.get vertexId positions
                |> Maybe.withDefault zeroV2

        x =
            V2.getX vertexPosition

        y =
            V2.getY vertexPosition

        velocity : (Vec2 -> Vec2 -> Vec2) -> Vec2 -> Vec2
        velocity force otherVertexPosition =
            V2.scale speed <| force vertexPosition otherVertexPosition

        -- Sum of repulsive forces from All vertices
        pushVector : Vec2
        pushVector =
            Graph.fold
                (\otherVertexId acc ->
                    let
                        otherVertexPosition =
                            Dict.get otherVertexId positions
                                |> Maybe.withDefault zeroV2
                    in
                    V2.add
                        (velocity (pushForce charge) otherVertexPosition)
                        acc
                )
                zeroV2
                graph

        -- Sum attractive forces from Connected vertices
        pullVector : Vec2
        pullVector =
            Graph.outgoingEdges vertexId graph
                |> List.foldr
                    (\otherVertexId acc ->
                        let
                            otherVertexPosition =
                                Dict.get otherVertexId positions
                                    |> Maybe.withDefault zeroV2
                        in
                        V2.add
                            (velocity (pullForce stiffness) otherVertexPosition)
                            acc
                    )
                    zeroV2
    in
    V2.vec2
        (x + V2.getX pushVector + V2.getX pullVector)
        (y + V2.getY pushVector + V2.getY pullVector)


pushForce : Float -> Vec2 -> Vec2 -> Vec2
pushForce charge v1 v2 =
    let
        diff =
            V2.sub v1 v2

        l =
            V2.length diff
    in
    if l > 0 then
        V2.scale (charge / l) (V2.normalize diff)

    else
        {- vertex doesn't affects itself -}
        zeroV2


pullForce : Float -> Vec2 -> Vec2 -> Vec2
pullForce stiffness v1 v2 =
    V2.scale stiffness <| V2.sub v2 v1


zeroV2 : Vec2
zeroV2 =
    V2.vec2 0 0
