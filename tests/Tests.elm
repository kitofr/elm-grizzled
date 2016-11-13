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
    { game | mission = Just (Preparation intensity) }


twoPlayers =
    NonEmptyList
        (Player (GrizzledCard Felix Rain) [] False [] [])
        [ (Player (GrizzledCard Lazare Shell) [] True [] []) ]


defaultGame =
    (Game
        twoPlayers
        InWar
        Nothing
        [ MerryChristmas ]
        [ (ThreatsCard (Card (NonEmptyList Rain []) False)) ]
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

                    intensity =
                        3

                    preparedGame =
                        preparation game intensity
                in
                    Expect.equal preparedGame.mission (Just (Preparation intensity))
        ]


all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        , missionTests
        ]
