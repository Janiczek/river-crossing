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

import Bag exposing (Bag)
import Graph exposing (Graph)
import Html exposing (Html)


type alias Problem =
    { initial : Graph Land ()
    , current : Graph Land ()
    , goal : Graph Land ()
    }


type alias Land =
    { entities : Bag Entity -- there can be multiple of each entity!
    , hasBoat : Bool
    }


{-| TODO do we want some more?

  - Fox, Goose, Bag of Beans?
  - Jealous Husbands?
  - Missionaries and Cannibals?
  - Bridge and Torch problem?
  - Boat weight problem?

More details here: <https://en.wikipedia.org/wiki/River_crossing_puzzle>

-}
type Entity
    = Wolf
    | Goat
    | Cabbage


main : Html msg
main =
    Html.text "TODO"
