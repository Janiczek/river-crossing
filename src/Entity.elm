module Entity exposing
    ( Entity(..)
    , isDangerousFor
    , toId
    , toString
    )

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


{-|

    Wolf |> isDangerousFor Goat
    --> True

-}
isDangerousFor : Entity -> Entity -> Bool
isDangerousFor prey predator =
    case ( predator, prey ) of
        ( Wolf, Goat ) ->
            True

        ( Goat, Cabbage ) ->
            True

        _ ->
            False


toId : Entity -> String
toId entity =
    case entity of
        Wolf ->
            "wolf"

        Goat ->
            "goat"

        Cabbage ->
            "cabbage"


toString : Entity -> String
toString entity =
    case entity of
        Wolf ->
            "Wolf"

        Goat ->
            "Goat"

        Cabbage ->
            "Cabbage"
