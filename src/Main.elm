module Main exposing (main)

{-| Game about the ["Wolf, goat and cabbage" problem](1)

In the original problem, you have two banks of a river, and a boat to sail on.
When you're with animals and the cabbage, nothing happens, but when they're left
on their own, they try to eat each other. The boat only has enough space for you
and one animal or the cabbage. There is a solution to how to transfer them so
that everybody's alive and well on the other side.

We can turn it up a notch: instead of two banks of a river (basically two
vertices and an edge), we can generalize to multiple vertices and edges, each
with some initial state, and then some goal state.

We can guarantee feasibility of the specific puzzles by starting from the goal
state and then randomly (or manually?) making valid moves until we're satisfied
with, I guess, the aesthetics of the "initial" state.

[1]: https://en.wikipedia.org/wiki/Wolf,_goat_and_cabbage_problem

-}

import Browser
import Entity exposing (Entity(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Land
import Level
import Problem exposing (Problem, ProblemState)
import Topology exposing (Topology)


type alias Model =
    { problem : Problem
    }


type Msg
    = ItemClicked
        { landId : Int
        , item : ItemClicked
        }


type ItemClicked
    = Entity Entity


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { problem = Level.canonical }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ItemClicked { landId, item } ->
            itemClicked landId item model


itemClicked : Int -> ItemClicked -> Model -> ( Model, Cmd Msg )
itemClicked landId item model =
    case item of
        Entity entity ->
            -- TODO
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view { problem } =
    { title = "River Crossing"
    , body =
        [ viewDotGraph problem.topology problem.current
        ]
    }


viewDotGraph : Topology -> ProblemState -> Html Msg
viewDotGraph topology state =
    Html.node "x-graphviz"
        [ Attrs.attribute "dot" <| Problem.toDot topology state
        , Events.on "x-graphviz-node-click" nodeClickDecoder
        ]
        []


nodeClickDecoder : Decoder Msg
nodeClickDecoder =
    Decode.field "detail" <|
        (Decode.field "id" Decode.string
            |> Decode.andThen
                (\string ->
                    (case String.split "." string of
                        [ landIdString, itemString ] ->
                            let
                                maybeItem : Maybe ItemClicked
                                maybeItem =
                                    case itemString of
                                        "wolf" ->
                                            Just <| Entity Wolf

                                        "goat" ->
                                            Just <| Entity Goat

                                        "cabbage" ->
                                            Just <| Entity Cabbage

                                        _ ->
                                            Nothing
                            in
                            Maybe.map2
                                (\landId item ->
                                    ItemClicked
                                        { landId = landId
                                        , item = item
                                        }
                                )
                                (Land.idFromString landIdString)
                                maybeItem

                        _ ->
                            Nothing
                    )
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "Bad format of clicked item ID")
                )
        )
