module Tests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList exposing (..)


nonEmptyListTests =
    describe "Non empty list"
        [ test "has at least one item" <|
            \() ->
                let
                    list =
                        NonEmptyList "a" []
                in
                    Expect.equal list.head "a"
        , test "asList creates a list" <|
            \() ->
                let
                    list =
                        NonEmptyList "a" [ "b" ]
                in
                    Expect.equal (asList list) [ "a", "b" ]
        ]


preparation : Game -> MissionIntensity -> Game
preparation game intensity =
    game


twoPlayers =
    NonEmptyList
        (Player (GrizzledCard Felix Rain) [] False [] [])
        [(Player (GrizzledCard Lazare Shell) [] True [] [])]


defaultGame =
    (Game
         twoPlayers
         InWar
         [ MerryChristmas ]
         [(ThreatsCard (Card (NonEmptyList Rain []) False)) ]
         []
         []
    )


missionTests =
    describe "Mission rules"
        [ test "Preparation " <|
            \() ->
                let
                    game =
                        defaultGame

                    preparedGame =
                        preparation game 3
                in
                    Expect.equal preparedGame.state InWar
        ]


all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        , missionTests
        ]
