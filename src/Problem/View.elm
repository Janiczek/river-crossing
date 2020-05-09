module Problem.View exposing (view)

import ForceLayout exposing (LayoutSettings)
import Html exposing (Html)
import Html.Attributes as Attrs
import Land exposing (Land)
import Level
import Math.Vector2 as V2 exposing (Vec2)
import Problem exposing (Problem, ProblemState)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import Topology exposing (Topology)


layoutSettings : LayoutSettings
layoutSettings =
    { charge = 10000
    , stiffness = 0.5
    , speed = 0.3
    }


view : Problem -> Html msg
view problem =
    let
        -- land ID to its (x,y) coords
        positions : List ( Int, Vec2 )
        positions =
            ForceLayout.findPositions
                layoutSettings
                Level.canonical.topology
                |> Debug.log "positions - TODO use"
    in
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
        [ Attrs.attribute "dot" <| Problem.toDot topology state ]
        []
