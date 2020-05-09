module Problem.View.ForceLayout exposing (viewForceLayout)

import AssocList as Dict exposing (Dict)
import ForceLayout exposing (LayoutSettings)
import Graph
import Html exposing (Html)
import Math.Vector2 as V2 exposing (Vec2)
import Problem exposing (ProblemState)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import Topology exposing (Topology)


layoutSettings : LayoutSettings
layoutSettings =
    { charge = 3000
    , stiffness = 0.5
    , speed = 0.02
    }


canvasWidth : Float
canvasWidth =
    320


canvasHeight : Float
canvasHeight =
    240


canvasCenter : Vec2
canvasCenter =
    V2.vec2
        (canvasWidth / 2)
        (canvasHeight / 2)


viewForceLayout : Topology -> ProblemState -> Html msg
viewForceLayout topology state =
    let
        -- land ID to its (x,y) coords
        positions : Dict Int Vec2
        positions =
            ForceLayout.findPositions layoutSettings topology
                |> Dict.map (\_ vec -> V2.add vec canvasCenter)

        width : String
        width =
            String.fromFloat canvasWidth

        height : String
        height =
            String.fromFloat canvasHeight
    in
    Svg.svg
        [ SvgAttrs.width width
        , SvgAttrs.height height
        , SvgAttrs.viewBox <| "0 0 " ++ width ++ " " ++ height
        ]
    <|
        viewEdges positions topology
            ++ viewVertices positions


viewEdges : Dict Int Vec2 -> Topology -> List (Svg msg)
viewEdges positions topology =
    List.map (viewEdge positions) <| Graph.edges topology


viewVertices : Dict Int Vec2 -> List (Svg msg)
viewVertices positions =
    List.map viewVertex (Dict.toList positions)


viewEdge : Dict Int Vec2 -> Graph.Edge Int () -> Svg msg
viewEdge positions ({ from, to } as edge) =
    let
        fromPosition =
            Dict.get from positions
                |> Maybe.withDefault (V2.vec2 0 0)

        toPosition =
            Dict.get to positions
                |> Maybe.withDefault (V2.vec2 0 0)
    in
    Svg.line
        [ SvgAttrs.x1 <| String.fromFloat <| V2.getX fromPosition
        , SvgAttrs.y1 <| String.fromFloat <| V2.getY fromPosition
        , SvgAttrs.x2 <| String.fromFloat <| V2.getX toPosition
        , SvgAttrs.y2 <| String.fromFloat <| V2.getY toPosition
        , SvgAttrs.strokeWidth "1"
        , SvgAttrs.stroke "#000"
        ]
        []


viewVertex : ( Int, Vec2 ) -> Svg msg
viewVertex ( id, position ) =
    let
        x =
            V2.getX position

        y =
            V2.getY position
    in
    Svg.g
        []
        [ Svg.circle
            [ SvgAttrs.cx <| String.fromFloat x
            , SvgAttrs.cy <| String.fromFloat y
            , SvgAttrs.r "10"
            ]
            []
        , Svg.text_
            [ SvgAttrs.x <| String.fromFloat <| x + 20
            , SvgAttrs.y <| String.fromFloat y
            ]
            [ Svg.text <| String.fromInt id ]
        ]
