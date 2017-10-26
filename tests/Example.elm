module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import OrderedDict


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


suite : Test
suite =
    describe "OrderedDict"
        [ describe "insertBefore"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> OrderedDict.insertBefore "b" ("d" => 4)
                        |> OrderedDict.insertBefore "a" ("e" => 5)
                        |> OrderedDict.insertBefore "e" ("c" => 6)
                        |> OrderedDict.toList
                        |> Expect.equalLists [ "c" => 6, "e" => 5, "a" => 1, "d" => 4, "b" => 2 ]
            ]
        , describe "insertAfter"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> OrderedDict.insertAfter "b" ("d" => 4)
                        |> OrderedDict.insertAfter "a" ("e" => 5)
                        |> OrderedDict.insertAfter "e" ("c" => 6)
                        |> OrderedDict.toList
                        |> Expect.equalLists [ "a" => 1, "e" => 5, "c" => 6, "b" => 2, "d" => 4 ]
            ]
        , describe "insertStart"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> OrderedDict.insertStart ("c" => 4)
                        |> OrderedDict.insertStart ("d" => 5)
                        |> OrderedDict.toList
                        |> Expect.equalLists [ "d" => 5, "c" => 4, "a" => 1, "b" => 2 ]
            ]
        , describe "insertEnd"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> OrderedDict.insertEnd ("c" => 4)
                        |> OrderedDict.insertEnd ("d" => 5)
                        |> OrderedDict.toList
                        |> Expect.equalLists [ "a" => 1, "b" => 2, "c" => 4, "d" => 5 ]
            ]
        , describe "remove"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> OrderedDict.remove "b"
                        |> OrderedDict.remove "c"
                        |> OrderedDict.toList
                        |> Expect.equalLists [ "a" => 1 ]
            ]
        , describe "get"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.get "b"
                                >> Expect.equal (Just 2)
                            , OrderedDict.get "not-found"
                                >> Expect.equal Nothing
                            ]
            ]
        , describe "getBefore"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.getBefore "b"
                                >> Expect.equal (Just 1)
                            , OrderedDict.getBefore "a"
                                >> Expect.equal Nothing
                            ]
            ]
        , describe "getAtIndex"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.getAtIndex 0
                                >> Expect.equal (Just 1)
                            , OrderedDict.getAtIndex 2
                                >> Expect.equal (Just 3)
                            , OrderedDict.getAtIndex 100
                                >> Expect.equal Nothing
                            ]
            ]
        , describe "getIndex"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.getIndex "a"
                                >> Expect.equal (Just 0)
                            , OrderedDict.getIndex "c"
                                >> Expect.equal (Just 2)
                            , OrderedDict.getIndex "not-found"
                                >> Expect.equal Nothing
                            ]
            ]
        , describe "getAfter"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.getAfter "b"
                                >> Expect.equal (Just 3)
                            , OrderedDict.getAfter "c"
                                >> Expect.equal Nothing
                            ]
            ]
        , describe "update"
            [ test "works" <|
                \_ ->
                    OrderedDict.fromList [ "a" => 1, "b" => 2, "c" => 3 ]
                        |> Expect.all
                            [ OrderedDict.update "a" (always Nothing)
                                >> OrderedDict.values
                                >> Expect.equalLists [ 2, 3 ]
                            , OrderedDict.update "a" (always (Just 42))
                                >> OrderedDict.values
                                >> Expect.equalLists [ 42, 2, 3 ]
                            , OrderedDict.update "not-found" (always (Just 42))
                                >> OrderedDict.values
                                >> Expect.equalLists [ 1, 2, 3 ]
                            ]
            ]
        , describe "fromList"
            [ describe "with size 0"
                [ test "is empty" <|
                    \_ ->
                        let
                            dict =
                                OrderedDict.fromList []
                        in
                            Expect.equalLists [] (OrderedDict.toList dict)
                ]
            , describe "with dupes"
                [ test "reinserts the latter dupe" <|
                    \_ ->
                        let
                            dict =
                                OrderedDict.fromList
                                    [ ( "a", 1 )
                                    , ( "b", 2 )
                                    , ( "c", 3 )
                                    , ( "a", 42 )
                                    ]
                        in
                            OrderedDict.toList dict
                                |> Expect.equalLists
                                    [ ( "b", 2 )
                                    , ( "c", 3 )
                                    , ( "a", 42 )
                                    ]
                ]
            ]
        ]
