module Tests exposing (..)

import Test exposing (..)
import UtilTests exposing (utilTests)
import NonEmptyListTests exposing (nonEmptyListTests)
import GameStateTests exposing (..)
import DealTests exposing (..)
import UpdatePlayerTests exposing (..)


all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        , missionTests
        , dealTests
        , utilTests
        , updatePlayerTests
        ]
