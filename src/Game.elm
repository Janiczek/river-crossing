module Game exposing (InteractionState(..))

import Entity exposing (Entity)


type InteractionState
    = DoingNothing
    | HoldingEntity
        { landId : Int
        , entity : Entity
        }
