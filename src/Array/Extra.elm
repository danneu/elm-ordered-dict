module Array.Extra exposing (..)

import Array exposing (Array)
import Util


insertBeforeMatch : (a -> Bool) -> a -> Array a -> Array a
insertBeforeMatch predicate value array =
    let
        step newArray queue =
            case Array.get 0 queue of
                Nothing ->
                    newArray

                Just item ->
                    let
                        nextQueue =
                            (Array.slice 1 (Array.length queue) queue)
                    in
                        if predicate item then
                            step (newArray |> Array.push value |> Array.push item) nextQueue
                        else
                            step (Array.push item newArray) nextQueue
    in
        step Array.empty array


insertAfterMatch : (a -> Bool) -> a -> Array a -> Array a
insertAfterMatch predicate value array =
    let
        step newArray queue =
            case Array.get 0 queue of
                Nothing ->
                    newArray

                Just item ->
                    let
                        nextQueue =
                            (Array.slice 1 (Array.length queue) queue)
                    in
                        if predicate item then
                            step (newArray |> Array.push item |> Array.push value) nextQueue
                        else
                            step (Array.push item newArray) nextQueue
    in
        step Array.empty array


findFirstMatchIndex : (a -> Bool) -> Array a -> Maybe Int
findFirstMatchIndex predicate array =
    let
        length =
            Array.length array

        step index =
            if index >= length then
                Nothing
            else
                let
                    currItem =
                        Array.get index array
                            |> Util.unwrapMaybe
                in
                    if predicate currItem then
                        Just index
                    else
                        step (index + 1)
    in
        step 0


{-| Slices an array but returns a tuple of (remaining, lost) items.
-}
partitionedSlice : Int -> Int -> Array a -> ( Array a, Array a )
partitionedSlice start end array =
    let
        length =
            Array.length array

        accumulate index kept lost =
            if index >= length then
                ( kept, lost )
            else
                let
                    item =
                        Array.get index array
                            |> Util.unwrapMaybe
                in
                    if index >= start && index < end then
                        accumulate (index + 1) (Array.push item kept) lost
                    else
                        accumulate (index + 1) kept (Array.push item lost)
    in
        accumulate 0 Array.empty Array.empty
