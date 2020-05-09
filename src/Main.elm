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

import AssocList as Dict exposing (Dict)
import Bag
import Browser
import Entity exposing (Entity(..))
import Game exposing (InteractionState(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Land
import Level
import List.Extra
import Problem exposing (Problem, ProblemState)
import Problem.Dot as Problem
import Topology exposing (Topology)


type alias Model =
    { problem : Problem
    , message : Maybe UserMessage
    , interactionState : InteractionState
    }


type Msg
    = ItemClicked
        { landId : Int
        , item : ItemClicked
        }


type ItemClicked
    = Entity Entity
    | Land


type UserMessage
    = CantMove
        { landId : Int
        , entity : Entity
        , reason : CantMoveReason
        }


type CantMoveReason
    = WouldStayAlone Entity Entity
    | DoesntHaveBoat
    | DoesntHaveFarmer


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
    ( { problem = Level.canonical
      , message = Nothing
      , interactionState = DoingNothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ItemClicked { landId, item } ->
            itemClicked landId item model


itemClicked : Int -> ItemClicked -> Model -> ( Model, Cmd Msg )
itemClicked landId item model =
    case ( model.interactionState, item ) of
        ( DoingNothing, Land ) ->
            ( model, Cmd.none )

        ( DoingNothing, Entity entity ) ->
            hold landId entity model

        ( HoldingEntity holded, Land ) ->
            if landId == holded.landId then
                doNothing model

            else
                tryToMoveTo landId holded model

        ( HoldingEntity holded, Entity entity ) ->
            if entity == holded.entity && landId == holded.landId then
                doNothing model

            else
                hold landId entity model


doNothing : Model -> ( Model, Cmd Msg )
doNothing model =
    ( { model | interactionState = DoingNothing }
    , Cmd.none
    )


hold : Int -> Entity -> Model -> ( Model, Cmd Msg )
hold landId entity model =
    ( { model
        | interactionState =
            HoldingEntity
                { landId = landId
                , entity = entity
                }
      }
    , Cmd.none
    )


tryToMoveTo : Int -> { landId : Int, entity : Entity } -> Model -> ( Model, Cmd Msg )
tryToMoveTo newLandId holded model =
    Dict.get holded.landId model.problem.current
        |> Maybe.map
            (\oldLand ->
                if not oldLand.hasFarmer then
                    ( { model
                        | interactionState = DoingNothing
                        , message =
                            Just <|
                                CantMove
                                    { landId = holded.landId
                                    , entity = holded.entity
                                    , reason = DoesntHaveFarmer
                                    }
                      }
                    , Cmd.none
                    )

                else if not oldLand.hasBoat then
                    ( { model
                        | interactionState = DoingNothing
                        , message =
                            Just <|
                                CantMove
                                    { landId = holded.landId
                                    , entity = holded.entity
                                    , reason = DoesntHaveBoat
                                    }
                      }
                    , Cmd.none
                    )

                else
                    let
                        entityPairs : List ( Entity, Entity )
                        entityPairs =
                            oldLand.entities
                                |> Bag.remove holded.entity
                                |> Bag.uniques
                                |> List.Extra.uniquePairs

                        dangerousToEachOther : ( Entity, Entity ) -> Bool
                        dangerousToEachOther ( e1, e2 ) =
                            Entity.isDangerousFor e1 e2
                                || Entity.isDangerousFor e2 e1
                    in
                    case List.filter dangerousToEachOther entityPairs of
                        [] ->
                            ( { model
                                | interactionState = DoingNothing
                                , problem =
                                    Problem.moveTo
                                        newLandId
                                        holded
                                        model.problem
                              }
                            , Cmd.none
                            )

                        ( e1, e2 ) :: _ ->
                            ( { model
                                | interactionState = DoingNothing
                                , message =
                                    Just <|
                                        CantMove
                                            { landId = holded.landId
                                            , entity = holded.entity
                                            , reason = WouldStayAlone e1 e2
                                            }
                              }
                            , Cmd.none
                            )
            )
        |> Maybe.withDefault ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view { problem, interactionState } =
    { title = "River Crossing"
    , body =
        [ viewDotGraph interactionState problem.topology problem.current
        ]
    }


viewDotGraph : InteractionState -> Topology -> ProblemState -> Html Msg
viewDotGraph interactionState topology state =
    Html.node "x-graphviz"
        [ Attrs.attribute "dot" <| Problem.toDot interactionState topology state
        , Events.on "x-graphviz-node-click" nodeClickDecoder
        ]
        []


nodeClickDecoder : Decoder Msg
nodeClickDecoder =
    Decode.field "detail" <|
        (Decode.field "id" Decode.string
            |> Decode.andThen
                (\string ->
                    (case String.split "." (Debug.log "string" string) of
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

                                        "land" ->
                                            Just Land

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
