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
        , test "fromList" <|
            \() ->
                [ "a", "b" ]
                    |> NEL.fromList
                    |> Expect.equal (Just (NonEmptyList "a" [ "b" ]))
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
        , test "merge" <|
            \() ->
                NonEmptyList "a" [ "b" ]
                    |> NEL.merge (NonEmptyList "c" [ "d" ])
                    |> NEL.asList
                    |> Expect.equal [ "a", "b", "c", "d" ]
        ]
