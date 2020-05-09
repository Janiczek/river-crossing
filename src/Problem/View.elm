module Problem.View exposing (view)

import AssocList as Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Land exposing (Land)
import Level
import Math.Vector2 as V2 exposing (Vec2)
import Problem exposing (Problem, ProblemState)
import Problem.View.ForceLayout as Problem
import Topology exposing (Topology)


view : Problem -> Html msg
view problem =
    Html.div []
        [ Html.div [] [ Html.text "DOT Graphviz representation:" ]
        , viewDotGraph problem.topology problem.current
        , Html.div [] [ Html.text "Clickable but not well laid out" ]
        , viewInteractive problem.topology problem.current
        , Html.div [] [ Html.text "Force layout output" ]
        , Problem.viewForceLayout problem.topology problem.current
        ]


viewDotGraph : Topology -> ProblemState -> Html msg
viewDotGraph topology state =
    Html.node "x-viz"
        [ Attrs.attribute "dot" <| Problem.toDot topology state ]
        []


viewInteractive : Topology -> ProblemState -> Html msg
viewInteractive topology state =
    state
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map viewLand
        |> Html.div []


viewLand : ( Int, Land ) -> Html msg
viewLand ( id, land ) =
    Html.text <| String.fromInt id
