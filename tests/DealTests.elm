module DealTests exposing (..)

import Test exposing (..)
import Expect
import GameState exposing (..)
import Types exposing (..)
import List exposing (..)
import TestHelpers exposing (..)


dealTests =
    describe "Dealing cards"
        [ test "Deal a list of cards to serveral players" <|
            \() ->
                [ felix, lazare ]
                    |> dealCards [ (threatCard Rain), (threatCard Winter), (threatCard Rain) ]
                    |> List.map .hand
                    |> Expect.equal [ [ (threatCard Rain), (threatCard Rain) ], [ (threatCard Winter) ] ]
        ]
