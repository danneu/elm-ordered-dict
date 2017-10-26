module Util exposing (..)


unwrapMaybe : Maybe a -> a
unwrapMaybe maybe =
    case maybe of
        Nothing ->
            Debug.crash "Tried to unwrap a Just"

        Just value ->
            value
