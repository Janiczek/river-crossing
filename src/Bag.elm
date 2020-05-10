module Bag exposing
    ( Bag
    , empty
    , eq
    , fromList
    , insert
    , remove
    , toCountedList
    , toList
    , uniques
    )

import AssocList as Dict exposing (Dict)


{-| Bag data structure, sometimes known as a Multiset.

This particular implementation uses the AssocList variant of Dict under the
hood, meaning that it can use any type as a Dict key (and transitively Set value
or Bag value).

Where `Set a` would be usually implemented as `Dict a ()`, bag or multiset would
be `Dict a Int` - we're counting how many times is an item present in the bag.

-}
type alias Bag a =
    Dict a Int


empty : Bag a
empty =
    Dict.empty


fromList : List a -> Bag a
fromList list =
    List.foldl
        insert
        empty
        list


toList : Bag a -> List a
toList bag =
    Dict.foldl
        (\key count acc -> List.append (List.repeat count key) acc)
        []
        bag


insert : a -> Bag a -> Bag a
insert value bag =
    Dict.update
        value
        (\maybeCount ->
            case maybeCount of
                Nothing ->
                    Just 1

                Just n ->
                    Just (n + 1)
        )
        bag


remove : a -> Bag a -> Bag a
remove value bag =
    Dict.update
        value
        (\maybeCount ->
            case maybeCount of
                Nothing ->
                    Nothing

                Just n ->
                    if n - 1 <= 0 then
                        {- If you've removed the last one, no need to hold the
                           zero there anymore...
                        -}
                        Nothing

                    else
                        Just (n - 1)
        )
        bag


uniques : Bag a -> List a
uniques bag =
    Dict.keys bag


toCountedList : Bag a -> List ( a, Int )
toCountedList bag =
    Dict.toList bag


eq : Bag a -> Bag a -> Bool
eq a b =
    Dict.eq a b
