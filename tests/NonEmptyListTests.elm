module NonEmptyListTests exposing (nonEmptyListTests)

import Test exposing (..)
import List exposing (..)
import Expect
import NonEmptyList as NEL exposing (..)


nonEmptyListTests =
    describe "Non empty list"
        [ test "has at least one item" <|
            \() ->
                NonEmptyList "a" []
                    |> NEL.head
                    |> Expect.equal "a"
        , test "asList creates a list" <|
            \() ->
                NonEmptyList "a" [ "b" ]
                    |> NEL.asList
                    |> Expect.equal [ "a", "b" ]
        , describe "fromList"
            [ test "with 0 elements" <|
                \() ->
                    []
                        |> NEL.fromList
                        |> Expect.equal (Nothing)
            , test "with 1 elements" <|
                \() ->
                    [ "a" ]
                        |> NEL.fromList
                        |> Expect.equal (Just (NonEmptyList "a" []))
            , test "with 2 elements" <|
                \() ->
                    [ "a", "b" ]
                        |> NEL.fromList
                        |> Expect.equal (Just (NonEmptyList "a" [ "b" ]))
            , test "with 3 elements" <|
                \() ->
                    [ "a", "b", "c" ]
                        |> NEL.fromList
                        |> Expect.equal (Just (NonEmptyList "a" [ "b", "c" ]))
            ]
        , test "count" <|
            \() ->
                NonEmptyList "a" [ "b", "c" ]
                    |> NEL.count
                    |> Expect.equal 3
        , test "add" <|
            \() ->
                NonEmptyList "a" [ "b" ]
                    |> NEL.add "c"
                    |> NEL.asList
                    |> Expect.equal [ "a", "b", "c" ]
        , describe "drop"
            [ test " 1 from ['a','b']" <|
                \() ->
                    NonEmptyList "a" [ "b" ]
                        |> NEL.drop 1
                        |> Expect.equal (Just (NonEmptyList "b" []))
            , test " 2 from ['a','b']" <|
                \() ->
                    NonEmptyList "a" [ "b" ]
                        |> NEL.drop 2
                        |> Expect.equal Nothing
            , test " 1 from ['a','b', 'c']" <|
                \() ->
                    NonEmptyList "a" [ "b", "c" ]
                        |> NEL.drop 1
                        |> Expect.equal (Just (NonEmptyList "b" [ "c" ]))
            ]
        , test "merge" <|
            \() ->
                NonEmptyList "a" [ "b" ]
                    |> NEL.merge (NonEmptyList "c" [ "d" ])
                    |> NEL.asList
                    |> Expect.equal [ "a", "b", "c", "d" ]
        ]
