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
import Bag exposing (Bag)
import Browser
import Entity exposing (Entity(..))
import Game
    exposing
        ( InteractionState(..)
        , Item(..)
        )
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
        , item : Item
        }
    | DismissMessage
    | Reset


type UserMessage
    = CantMove
        { oldLandId : Int
        , newLandId : Int
        , item : Item
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

        DismissMessage ->
            ( { model | message = Nothing }
            , Cmd.none
            )

        Reset ->
            ( { model | problem = Problem.reset model.problem }
            , Cmd.none
            )


itemClicked : Int -> Item -> Model -> ( Model, Cmd Msg )
itemClicked landId item model =
    case ( model.interactionState, item ) of
        ( DoingNothing, Land ) ->
            ( model, Cmd.none )

        ( DoingNothing, Farmer ) ->
            if Problem.landHasBoats landId model.problem then
                hold landId item model

            else
                doNothing model

        ( DoingNothing, Entity entity ) ->
            if
                Problem.landHasFarmers landId model.problem
                    && Problem.landHasBoats landId model.problem
            then
                hold landId item model

            else
                doNothing model

        ( HoldingItem holded, Land ) ->
            if landId == holded.landId then
                doNothing model

            else
                tryToMoveTo landId holded model

        ( HoldingItem holded, Farmer ) ->
            if
                Problem.landHasBoats landId model.problem
                    && (holded /= { landId = landId, item = Farmer })
            then
                hold landId item model

            else
                doNothing model

        ( HoldingItem holded, Entity entity ) ->
            if item == holded.item && landId == holded.landId then
                doNothing model

            else
                hold landId item model


doNothing : Model -> ( Model, Cmd Msg )
doNothing model =
    ( { model | interactionState = DoingNothing }
    , Cmd.none
    )


hold : Int -> Item -> Model -> ( Model, Cmd Msg )
hold landId item model =
    ( { model
        | interactionState =
            HoldingItem
                { landId = landId
                , item = item
                }
        , message = Nothing
      }
    , Cmd.none
    )


tryToMoveTo : Int -> { landId : Int, item : Item } -> Model -> ( Model, Cmd Msg )
tryToMoveTo newLandId holded model =
    Dict.get holded.landId model.problem.current
        |> Maybe.map
            (\oldLand ->
                if oldLand.farmers <= 0 then
                    ( { model
                        | interactionState = DoingNothing
                        , message =
                            Just <|
                                CantMove
                                    { oldLandId = holded.landId
                                    , newLandId = newLandId
                                    , item = holded.item
                                    , reason = DoesntHaveFarmer
                                    }
                      }
                    , Cmd.none
                    )

                else if oldLand.boats <= 0 then
                    ( { model
                        | interactionState = DoingNothing
                        , message =
                            Just <|
                                CantMove
                                    { oldLandId = holded.landId
                                    , newLandId = newLandId
                                    , item = holded.item
                                    , reason = DoesntHaveBoat
                                    }
                      }
                    , Cmd.none
                    )

                else
                    let
                        bagWithoutEntity : Bag Entity
                        bagWithoutEntity =
                            case holded.item of
                                Entity entity ->
                                    oldLand.entities
                                        |> Bag.remove entity

                                Farmer ->
                                    oldLand.entities

                                Land ->
                                    {- a bit of Impossible states being possible
                                       here, with the Land being a possible Item
                                       to hold... TODO
                                    -}
                                    oldLand.entities

                        entityPairs : List ( Entity, Entity )
                        entityPairs =
                            bagWithoutEntity
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
                                            { oldLandId = holded.landId
                                            , newLandId = newLandId
                                            , item = holded.item
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
view model =
    { title = "River Crossing"
    , body =
        if Problem.hasWon model.problem then
            viewWin

        else
            viewGame model
    }


viewGame : Model -> List (Html Msg)
viewGame { problem, interactionState, message } =
    [ Html.div [ Attrs.class "game" ]
        [ viewDescription
        , viewReset problem
        , viewDotGraph interactionState problem
        , viewMessage message
        ]
    ]


viewDescription : Html Msg
viewDescription =
    Html.div []
        [ Html.h1 [] [ Html.text "River Crossing" ]
        , Html.p [] [ Html.text "TODO write a description :)" ]
        ]


viewWin : List (Html Msg)
viewWin =
    [ Html.text "You won!!!" ]


viewReset : Problem -> Html Msg
viewReset problem =
    Html.button
        [ Attrs.disabled <| problem.current == problem.initial
        , Attrs.class "reset-button"
        , Events.onClick Reset
        ]
        [ Html.text "Reset" ]


viewMessage : Maybe UserMessage -> Html Msg
viewMessage maybeMessage =
    case maybeMessage of
        Nothing ->
            Html.text ""

        Just message ->
            Html.div []
                [ Html.span [] [ Html.text <| messageToString message ]
                , Html.span
                    [ Attrs.class "dismiss"
                    , Events.onClick DismissMessage
                    ]
                    [ Html.text "Dismiss" ]
                ]


messageToString : UserMessage -> String
messageToString message =
    case message of
        CantMove { oldLandId, newLandId, item, reason } ->
            "Can't move "
                ++ Game.itemToString item
                ++ " from land "
                ++ String.fromInt oldLandId
                ++ " to land "
                ++ String.fromInt newLandId
                ++ ", because "
                ++ cantMoveReasonToString reason


cantMoveReasonToString : CantMoveReason -> String
cantMoveReasonToString reason =
    case reason of
        WouldStayAlone e1 e2 ->
            Entity.toString e1
                ++ " and "
                ++ Entity.toString e2
                ++ " would stay alone!"

        DoesntHaveBoat ->
            "there is no boat to sail on!"

        DoesntHaveFarmer ->
            "the farmer is not there!"


viewDotGraph : InteractionState -> Problem -> Html Msg
viewDotGraph interactionState problem =
    Html.node "x-graphviz"
        [ Attrs.attribute "dot" <| Problem.toDot interactionState problem
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
                            Maybe.map2
                                (\landId item ->
                                    ItemClicked
                                        { landId = landId
                                        , item = item
                                        }
                                )
                                (Land.idFromString landIdString)
                                (Game.itemFromString itemString)

                        _ ->
                            Nothing
                    )
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "Bad format of clicked item ID")
                )
        )
