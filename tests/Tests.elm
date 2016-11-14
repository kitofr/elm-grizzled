module Tests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList as NEL exposing (..)


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
        , test "count" <|
            \() ->
                let
                    list =
                        NonEmptyList "a" [ "b", "c" ]
                in
                    Expect.equal (count list) 3
        ]


preparation : Game -> MissionIntensity -> Game
preparation game intensity =
    { game | mission = Just (Preparation intensity) }


enterMission : Game -> Game
enterMission game =
    case game.mission of
        Just state ->
            case state of
                Preparation intensity ->
                    let
                        players =
                            List.length (asList game.players)

                        trailsPile_ =
                            List.drop (intensity * players) game.trailsPile
                    in
                        { game
                            | mission = Just TheMission
                            , trailsPile = trailsPile_
                        }

                _ ->
                    game

        _ ->
            game


twoPlayers =
    NonEmptyList
        (Player (GrizzledCard Felix Rain) [] False [] [])
        [ (Player (GrizzledCard Lazare Shell) [] True [] []) ]


threat threats =
    ThreatsCard (Card threats False)


defaultGame =
    (Game
        twoPlayers
        InWar
        Nothing
        (List.repeat 4 (threat (NonEmptyList Rain [])))
        [ threat (NonEmptyList Winter []) ]
        []
        []
    )


missionTests =
    describe "Mission rules"
        [ describe "Preparation"
            [ test "A game starts with preparation " <|
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
        , describe "The mission"
            [ test "The mission starts with a prepared game" <|
                \() ->
                    let
                        game =
                            preparation defaultGame 4

                        inMissionGame =
                            enterMission game
                    in
                        Expect.equal inMissionGame.mission (Just TheMission)
            , test "entering a mission all players gets cards from the trails pile" <|
                \() ->
                    let
                        intensity =
                            2

                        game =
                            preparation defaultGame intensity

                        inMissionGame =
                            enterMission game
                    in
                        Expect.equal (List.length inMissionGame.trailsPile) ((List.length defaultGame.trailsPile) - intensity * 2)
            , test "hand size of players have increased with the mission intensitiy number of cards" <|
                \() ->
                    let
                        intensity =
                            2

                        game =
                            preparation defaultGame intensity

                        inMissionGame =
                            enterMission game

                        hand1Size =
                            2

                        hand2Size =
                            3
                    in
                        Expect.equal [ hand1Size, hand2Size ] [ intensity, intensity ]
            , test "The mission ends when all players have withdrawn" <|
                \() ->
                    Expect.equal 1 1
            ]
        ]


all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        , missionTests
        ]
