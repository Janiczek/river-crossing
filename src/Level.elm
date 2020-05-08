module Level exposing (..)

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
                    , hasBoat = True
                    , hasFarmer = True
                    }
                  )
                , ( 2
                  , { entities = Bag.empty
                    , hasBoat = False
                    , hasFarmer = False
                    }
                  )
                ]
        , goal =
            Dict.fromList
                [ ( 1
                  , { entities = Bag.empty
                    , hasBoat = False
                    , hasFarmer = False
                    }
                  )
                , ( 2
                  , { entities = Bag.fromList [ Wolf, Goat, Cabbage ]
                    , hasBoat = True
                    , hasFarmer = True
                    }
                  )
                ]
        }
