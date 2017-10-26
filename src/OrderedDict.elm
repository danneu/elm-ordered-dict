module OrderedDict
    exposing
        ( OrderedDict
        , empty
        , singleton
        , fromList
        , toList
        , toArray
        , values
        , foldl
        , foldr
        , map
        , insertBefore
        , insertAfter
        , insertStart
        , insertEnd
        , remove
        , isEmpty
        , keys
        , size
        , member
        , partition
        , filter
        , get
        , getBefore
        , getAfter
        , getAtIndex
        , getIndex
        , update
        , slice
        )

{-| A dictionary with ordered keys. Useful for when you have sequential
data and want the ergonomics of a Dict.


# OrderedDict

@docs OrderedDict


# Build

@docs empty, singleton, fromList


# Query

@docs member, get, getBefore, getAfter, getIndex, getAtIndex, isEmpty, size


# Update

@docs remove, insertStart, insertEnd, insertBefore, insertAfter, update, slice


# Transform

@docs toArray, toList, values, foldl, foldr, map, keys, partition, filter

-}

import Array.Extra
import Array exposing (Array)
import Dict exposing (Dict)
import Util


{-| OrderedDict is a Dict with insertion order.
-}
type OrderedDict comparable v
    = OrderedDict
        { items : Dict comparable v
        , keys : Array comparable
        }



-- BUILD


{-| Create an empty dict.
-}
empty : OrderedDict comparable v
empty =
    OrderedDict
        { items = Dict.empty
        , keys = Array.empty
        }


{-| Create a dict with one key/value pair.
-}
singleton : comparable -> v -> OrderedDict comparable v
singleton k v =
    OrderedDict
        { items = Dict.singleton k v
        , keys = Array.fromList [ k ]
        }


{-| Create a dict from a list of pairs.
-}
fromList : List ( comparable, v ) -> OrderedDict comparable v
fromList pairs =
    List.foldl (\pair odict -> insertEnd pair odict) empty pairs



-- QUERY


{-| Check if dict has a given key.
-}
member : comparable -> OrderedDict comparable v -> Bool
member key (OrderedDict { items }) =
    Dict.member key items


{-| Get value for given key.
-}
get : comparable -> OrderedDict comparable v -> Maybe v
get key (OrderedDict { keys, items }) =
    Dict.get key items


getKeyAfter : comparable -> OrderedDict comparable v -> Maybe comparable
getKeyAfter key ((OrderedDict { keys, items }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstMatchIndex (\k -> k == key) keys)
        |> Maybe.andThen (\index -> Array.get (index + 1) keys)


{-| Get value of the key that comes after given key.
-}
getAfter : comparable -> OrderedDict comparable v -> Maybe v
getAfter key ((OrderedDict { keys, items }) as odict) =
    getKeyAfter key odict
        |> Maybe.andThen (\after -> Dict.get after items)


getKeyBefore : comparable -> OrderedDict comparable a -> Maybe comparable
getKeyBefore key ((OrderedDict { keys, items }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstMatchIndex (\k -> k == key) keys)
        |> Maybe.andThen (\index -> Array.get (index - 1) keys)


{-| Get value of the key that comes before given key.
-}
getBefore : comparable -> OrderedDict comparable a -> Maybe a
getBefore key ((OrderedDict { keys, items }) as odict) =
    getKeyBefore key odict
        |> Maybe.andThen (\before -> Dict.get before items)


{-| Get the index of a key.
-}
getIndex : comparable -> OrderedDict comparable v -> Maybe Int
getIndex key ((OrderedDict { keys }) as odict) =
    get key odict
        |> Maybe.andThen (\_ -> Array.Extra.findFirstMatchIndex (\k -> k == key) keys)


{-| Get the value at a given index.
-}
getAtIndex : Int -> OrderedDict comparable v -> Maybe v
getAtIndex index (OrderedDict { items, keys }) =
    keys
        |> Array.get index
        |> Maybe.andThen (\key -> Dict.get key items)


{-| Check if a dict has no pairs.
-}
isEmpty : OrderedDict comparable v -> Bool
isEmpty (OrderedDict { keys }) =
    Array.isEmpty keys


{-| Get the number of contained items.
-}
size : OrderedDict comparable v -> Int
size (OrderedDict { keys }) =
    Array.length keys



-- UPDATE


{-| Returns a sliced subset of the dict. The slice indexes are
start-inclusive and end-exclusive.
-}
slice : Int -> Int -> OrderedDict comparable v -> OrderedDict comparable v
slice start end (OrderedDict { items, keys }) =
    let
        ( keptKeys, lostKeys ) =
            Array.Extra.partitionedSlice start end keys

        -- Remove the keys that got sliced out
        nextItems =
            Array.foldl (\key acc -> Dict.remove key acc) items lostKeys
    in
        OrderedDict { items = nextItems, keys = keptKeys }


{-| Remove a key from a dict.
-}
remove : comparable -> OrderedDict comparable v -> OrderedDict comparable v
remove key ((OrderedDict { items, keys }) as odict) =
    case Dict.get key items of
        Nothing ->
            odict

        Just _ ->
            OrderedDict
                { items = Dict.remove key items
                , keys = Array.filter (\k -> k /= key) keys
                }


{-| If insertion key already exists, then it is updated and moved.
-}
insertBefore : comparable -> ( comparable, v ) -> OrderedDict comparable v -> OrderedDict comparable v
insertBefore marker ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.Extra.insertBeforeMatch (\k -> k == marker) key
    in
        OrderedDict { items = nextItems, keys = nextKeys }


{-| If insertion key already exists, then it is updated and moved.
-}
insertAfter : comparable -> ( comparable, v ) -> OrderedDict comparable v -> OrderedDict comparable v
insertAfter marker ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.Extra.insertAfterMatch (\k -> k == marker) key
    in
        OrderedDict { items = nextItems, keys = nextKeys }


{-| Insert a pair at the start (left-hand side).
-}
insertStart : ( comparable, v ) -> OrderedDict comparable v -> OrderedDict comparable v
insertStart ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.append (Array.fromList [ key ])
    in
        OrderedDict { items = nextItems, keys = nextKeys }


{-| Insert a pair at the end (right-hand side).
-}
insertEnd : ( comparable, v ) -> OrderedDict comparable v -> OrderedDict comparable v
insertEnd ( key, value ) ((OrderedDict { items, keys }) as odict) =
    let
        nextItems =
            Dict.insert key value items

        nextKeys =
            keys
                |> Array.filter (\k -> k /= key)
                |> Array.push key
    in
        OrderedDict { items = nextItems, keys = nextKeys }


{-| Transform an existing value. Returning `Nothing` will remove the existing key.
-}
update : comparable -> (Maybe v -> Maybe v) -> OrderedDict comparable v -> OrderedDict comparable v
update key update ((OrderedDict ({ items, keys } as state)) as odict) =
    case update (get key odict) of
        Nothing ->
            remove key odict

        Just newValue ->
            let
                nextItems =
                    Dict.insert key newValue state.items
            in
                OrderedDict { state | items = nextItems }



-- TRANSFORM


{-| Get an array of pairs.
-}
toArray : OrderedDict comparable v -> Array ( comparable, v )
toArray (OrderedDict { items, keys }) =
    Array.map
        (\k -> ( k, Util.unwrapMaybe (Dict.get k items) ))
        keys


{-| Get a list of pairs.
-}
toList : OrderedDict comparable v -> List ( comparable, v )
toList =
    Array.toList << toArray


{-| Get a list of keys.
-}
keys : OrderedDict comparable v -> List comparable
keys (OrderedDict { keys }) =
    Array.toList keys


{-| Get a list of values.
-}
values : OrderedDict comparable v -> List v
values (OrderedDict { items, keys }) =
    keys
        |> Array.map (\k -> Util.unwrapMaybe (Dict.get k items))
        |> Array.toList


{-| Reduce a dict from the left.
-}
foldl : (a -> b -> b) -> b -> OrderedDict comparable a -> b
foldl step init odict =
    odict
        |> values
        |> List.foldl step init


{-| Reduce a dict from the right.
-}
foldr : (a -> b -> b) -> b -> OrderedDict comparable a -> b
foldr step init odict =
    odict
        |> values
        |> List.foldr step init


{-| Apply a function to all values in the dictionary.
-}
map : (comparable -> a -> b) -> OrderedDict comparable a -> OrderedDict comparable b
map f (OrderedDict ({ items, keys } as state)) =
    OrderedDict
        { state | items = Dict.map f items }


{-| Get a dict of pairs that satisfy a predicate.
-}
filter : (comparable -> v -> Bool) -> OrderedDict comparable v -> OrderedDict comparable v
filter predicate (OrderedDict { items, keys }) =
    Array.foldl
        (\k acc ->
            let
                v =
                    Util.unwrapMaybe (Dict.get k items)
            in
                if predicate k v then
                    insertEnd ( k, v ) acc
                else
                    acc
        )
        empty
        keys


{-| The left dict contains pairs that satisfied the predicate, the right dict contains
pairs that failed the predicate.
-}
partition : (comparable -> v -> Bool) -> OrderedDict comparable v -> ( OrderedDict comparable v, OrderedDict comparable v )
partition predicate (OrderedDict { items, keys }) =
    Array.foldl
        (\k ( pass, fail ) ->
            let
                v =
                    Util.unwrapMaybe (Dict.get k items)
            in
                if predicate k v then
                    ( insertEnd ( k, v ) pass, fail )
                else
                    ( pass, insertEnd ( k, v ) fail )
        )
        ( empty, empty )
        keys
