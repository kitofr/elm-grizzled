module UtilTests exposing (utilTests)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Util exposing (cycleBy)


utilTests =
    describe "CycleBy"
        [ test "Partition a list by 2" <|
            \() ->
                [ "a", "b", "c" ]
                    |> cycleBy 2
                    |> Expect.equal [ [ "a", "c" ], [ "b" ] ]
        , test "Partition a list by 3" <|
            \() ->
                [ "a", "b", "c" ]
                    |> cycleBy 3
                    |> Expect.equal [ [ "a" ], [ "b" ], [ "c" ] ]
        , test "Partition 6 items by 3" <|
            \() ->
                [ "a", "b", "c", "d", "e", "f" ]
                    |> cycleBy 3
                    |> Expect.equal [ [ "a", "d" ], [ "b", "e" ], [ "c", "f" ] ]
        , test "Partition 5 items by 3" <|
            \() ->
                [ "a", "b", "c", "d", "e" ]
                    |> cycleBy 3
                    |> Expect.equal [ [ "a", "d" ], [ "b", "e" ], [ "c" ] ]
        ]
