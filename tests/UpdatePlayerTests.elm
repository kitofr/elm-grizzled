module UpdatePlayerTests exposing (..)

import Test exposing (..)
import Expect
import NonEmptyList as NEL exposing (..)
import GameState exposing (..)
import TestHelpers exposing (..)

updatePlayerTests =
    describe "Update player"
        [ test "A player can get a speach token" <|
            \() ->
                let
                    list =
                        twoPlayers

                    player =
                        { lazare | speachTokens = [ Nothing ] }
                in
                    Expect.equal (updatePlayer list player) (NonEmptyList felix [ player ])
        ]



