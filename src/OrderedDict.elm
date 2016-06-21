
module OrderedDict exposing
  ( OrderedDict
  , empty, singleton, insert, update, remove
  , isEmpty, member, get, size
  , keys, values, toList, fromList
  , map, foldl, foldr, filter, partition
  , union, intersect, diff, merge
  , compact
  )

{-| An ordered dictionary is always iterated in insertion order
and preserves that order when converted into other collections.

Has the same API has Elm's core Dict type.

- Inserting a new key puts it at the end.
- Removing a key and then re-inserting it puts it at the end.
- Updating the value of an existing key does not change its order.

# Insertion-Ordered Dictionaries
@docs OrderedDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

# Special
@docs compact

-}

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Set

-- Private
type alias OrderedDictRecord k v =
  { items : Dict k Int         -- maps items to index
  , indexes : Array (Maybe v)  -- tracks item ordering
  }

{-| The opaque set representation.
-}
type alias OrderedDict k v = OrderedDictRecord k v

--
-- CREATE
--

{-| Create an empty dict.
-}
empty : OrderedDict k v
empty =
  { items = Dict.empty
  , indexes = Array.empty
  }

{-| Create a dict with one pair.
-}
singleton : comparable -> v -> OrderedDict comparable v
singleton k v =
  insert k v empty

{-| Insert a pair.

If key exists, the value is replaced without changing insertion order.
-}
insert : comparable -> v -> OrderedDict comparable v -> OrderedDict comparable v
insert k v dict =
  if Dict.member k dict.items then
    dict
  else
    { dict | items = Dict.insert k (Array.length dict.indexes) dict.items
           , indexes = Array.push (Just v) dict.indexes
    }

{-| Update the value of a key. If you return Nothing, the key is removed.
-}
update : comparable -> (Maybe v -> Maybe v) -> OrderedDict comparable v -> OrderedDict comparable v
update k xform dict =
  case Dict.get k dict.items of
    -- Item not in dict
    Nothing ->
      case xform Nothing of
        -- User wants to do nothing
        Nothing ->
          dict
        -- User wants to insert a value
        Just next ->
          insert k next dict
    -- Item in dict
    Just index ->
      case Array.get index dict.indexes of
        -- Index out of range -- cannot happen
        Nothing ->
          Debug.crash "Impossible"
        -- Index in range
        Just slot ->
          case slot of
            -- User is accessing a Nothing hole -- cannot happen
            Nothing ->
              Debug.crash "Impossible"
            -- User is accessing the previous value
            Just prev ->
              case xform (Just prev) of
                -- User wants to remove the pair
                Nothing ->
                  remove k dict
                -- User is replacing the pair with a new value
                Just next ->
                  { dict | indexes = Array.set index (Just next) dict.indexes }

{-| Remove a pair.
-}
remove : comparable -> OrderedDict comparable v -> OrderedDict comparable v
remove k dict =
  case Dict.get k dict.items of
    Nothing ->
      dict
    Just index ->
      { dict | items = Dict.remove k dict.items
             , indexes = Array.set index Nothing dict.indexes
      }

--
-- QUERY
--

{-| Check if a dict has no keys.
-}
isEmpty : OrderedDict k v -> Bool
isEmpty dict =
  Dict.isEmpty dict.items

{-| Check if a dict contains a key.
-}
member : comparable -> OrderedDict comparable v -> Bool
member k dict =
  Dict.member k dict.items

{-| Get value of key
-}
get : comparable -> OrderedDict comparable v -> Maybe v
get k dict =
  case Dict.get k dict.items of
    Nothing ->
      Nothing
    Just index ->
      case Array.get index dict.indexes of
        -- Index is out of bounds
        Nothing ->
          Debug.crash "Impossible"
        Just slot ->
          case slot of
            -- User is accessing a Nothing slot
            Nothing ->
              Debug.crash "Impossible"
            Just v ->
              Just v

{-| Count the keys of a dict.
-}
size : OrderedDict k v -> Int
size dict =
  Dict.size dict.items

--
-- LISTS
--

{-| Get list of keys in insertion order.
-}
keys : OrderedDict comparable v -> List comparable
keys dict =
  Dict.toList dict.items
  |> List.sortBy snd
  |> List.map fst

{-| Get list of values in insertion order.
-}
values : OrderedDict comparable v -> List v
values dict =
  keys dict
  |> List.map (\k -> forceUnwrap (get k dict))

{-| Get list of (key, value) tuples.
-}
toList : OrderedDict comparable v -> List (comparable, v)
toList dict =
  List.map2 (,) (keys dict) (values dict)

{-| Create dict from list of (key, value) tuple.
-}
fromList : List (comparable, v) -> OrderedDict comparable v
fromList list =
  List.foldl (\ (k, v) memo -> insert k v memo) empty list

--
-- TRANSFORM
--

{-| Map a function over the pairs of a dict in insertion order,
letting you update the value of each pair.
-}
map : (comparable -> v -> v2) -> OrderedDict comparable v -> OrderedDict comparable v2
map xform =
  fromList << List.map (\ (k, v) -> (k, xform k v)) << toList

{-| Reduce a dict in insertion order from the left
-}
foldl : (comparable -> v -> b -> b) -> b -> OrderedDict comparable v -> b
foldl accum init dict =
  List.foldl (\ (k, v) memo -> accum k v memo) init (toList dict)

{-| Reduce a dict in insertion order from the right
-}
foldr : (comparable -> v -> b -> b) -> b -> OrderedDict comparable v -> b
foldr accum init dict =
  List.foldr (\ (k, v) memo -> accum k v memo) init (toList dict)

{-| Keep the pairs that pass the predicate.
-}
filter : (comparable -> v -> Bool) -> OrderedDict comparable v -> OrderedDict comparable v
filter pred dict =
  let
    accum k v memo =
      if pred k v then
        insert k v memo
      else
        memo
  in
    foldl accum empty dict

{-| Apply a predicate to each pair,
returning a tuple of dicts (pairsThatPass, pairsThatFail).
-}
partition : (comparable -> v -> Bool) -> OrderedDict comparable v -> (OrderedDict comparable v, OrderedDict comparable v)
partition pred dict =
  let
    accum k v (yes, no) =
      if pred k v then
        (insert k v yes, no)
      else
        (yes, insert k v no)
  in
    foldl accum (empty, empty) dict

--
-- COMBINE
--

{-| Merge the left dictionary into the right dictionary.

If there are collisions, the left value is used.
-}
union : OrderedDict comparable v -> OrderedDict comparable v -> OrderedDict comparable v
union left right =
  foldl insert right left

{-| Keep pairs in the left dictionary when their key appears
in the right dictionary.
-}
intersect : OrderedDict comparable v -> OrderedDict comparable v -> OrderedDict comparable v
intersect left right =
  filter (\k v -> member k right) left

{-| Keep pairs in the left dictionary when their key does not
appear in the right dictionary.
-}
diff : OrderedDict comparable v -> OrderedDict comparable v -> OrderedDict comparable v
diff left right =
  filter (\k v -> not (member k right)) left

{-| The most general way of combining two dictionaries.

You provide three accumulators for when a given key appears:

1. Only in the left dictionary.
2. In both dictionaries.
3. Only in the right dictionary.

The keys are traversed in the insertion order of the left dictionary
and then the right dictionary.
-}
merge :  (comparable -> a -> result -> result)
      -> (comparable -> a -> b -> result -> result)
      -> (comparable -> b -> result -> result)
      -> OrderedDict comparable a
      -> OrderedDict comparable b
      -> result
      -> result
merge accumLeft accumBoth accumRight left right init =
  let
    allKeys = unique (List.concat [(keys left), (keys right)])
    accum k result =
      case (get k left, get k right) of
        (Just v1, Nothing) -> accumLeft k v1 result
        (Just v1, Just v2) -> accumBoth k v1 v2 result
        (Nothing, Just v2) -> accumRight k v2 result
        (Nothing, Nothing) -> Debug.crash "Impossible"
  in
    List.foldl accum init allKeys

--
-- SPECIAL
--

{-| Clean up the internal datastructure.

As `remove` is called on a dict, holes are left behind
in the underlying datastructure. It's possible to have unbounded
memory growth in the rare case where you call `remove` a substantial
amount of times over a long period of time on a single dict without
calling any of the functions on it that return a new dict (thus
resetting the holes).
-}
compact : OrderedDict comparable v -> OrderedDict comparable v
compact dict =
  foldl insert empty dict

--
-- PRIVATE HELPERS
--

{- Keeps the first occurrence of each value.
-}
unique : List comparable -> List comparable
unique list =
  let
    accum v (seen, memo) =
      if Set.member v seen then
        (seen, memo)
      else
        (Set.insert v seen, List.append memo [v])
  in
    snd <| List.foldl accum (Set.empty, []) list

forceUnwrap : Maybe a -> a
forceUnwrap maybe =
  case maybe of
    Just val -> val
    Nothing -> Debug.crash "Impossible"
