module Level exposing (canonical)

import AssocList as Dict exposing (Dict)
import Bag
import Entity exposing (Entity(..))
import Graph
import Land
import Problem exposing (Problem)
import Topology


canonical : Problem
canonical =
    Problem.init
        { topology = Topology.fromEdges [ ( 1, 2 ) ]
        , initial =
            Dict.fromList
                [ ( 1
                  , { entities = Bag.fromList [ Wolf, Goat, Cabbage ]
                    , boats = 1
                    , farmers = 1
                    }
                  )
                , ( 2
                  , { entities = Bag.empty
                    , boats = 0
                    , farmers = 0
                    }
                  )
                ]
        , goal =
            Dict.fromList
                [ ( 1
                  , { entities = Bag.empty
                    , boats = 0
                    , farmers = 0
                    }
                  )
                , ( 2
                  , { entities = Bag.fromList [ Wolf, Goat, Cabbage ]
                    , boats = 1
                    , farmers = 1
                    }
                  )
                ]
        }
