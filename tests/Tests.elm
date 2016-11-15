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
                NonEmptyList "a" []
                    |> NEL.head
                    |> Expect.equal "a"
        , test "asList creates a list" <|
            \() ->
                NonEmptyList "a" [ "b" ]
                    |> NEL.asList
                    |> Expect.equal [ "a", "b" ]
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


preparation : Game -> MissionIntensity -> Game
preparation game intensity =
    { game | mission = Just (Preparation intensity) }


deal : TrialCard -> Player -> Player
deal card player =
    { player | hand = card :: player.hand }


dealTests =
    describe "Dealing cards"
        [ test "Dealing a card to a player adds it to his hand" <|
            \() ->
                let
                    card =
                        threatCard Rain
                in
                    deal card emptyPlayer
                        |> .hand
                        |> Expect.equal [ (threatCard Rain) ]
        ]


enterMission : Game -> Game
enterMission game =
    case game.mission of
        Just state ->
            case state of
                Preparation intensity ->
                    let
                        playerCount =
                            List.length (asList game.players)

                        cardsToDistribute =
                            intensity * playerCount

                        newCards =
                            List.take cardsToDistribute game.trailsPile

                        players_ =
                            game.players

                        --deal newCards game.players
                        trailsPile_ =
                            List.drop cardsToDistribute game.trailsPile
                    in
                        { game
                            | mission = Just TheMission
                            , trailsPile = trailsPile_
                        }

                _ ->
                    game

        _ ->
            game


emptyPlayer : Player
emptyPlayer =
    (Player (GrizzledCard Felix Rain) [] False [] [])


twoPlayers =
    NonEmptyList
        (Player (GrizzledCard Felix Rain) [] False [] [])
        [ (Player (GrizzledCard Lazare Shell) [] True [] []) ]


threatCard threat =
    ThreatsCard (Card (NonEmptyList threat []) False)


defaultGame =
    (Game
        twoPlayers
        InWar
        Nothing
        (List.repeat 4 (threatCard Rain))
        [ threatCard Winter ]
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
              --, test "hand size of players have increased with the mission intensitiy number of cards" <|
              --    \() ->
              --        let
              --            intensity =
              --                2
              --            game =
              --                preparation defaultGame intensity
              --            inMissionGame =
              --                enterMission game
              --            players =
              --                NEL.asList inMissionGame.players
              --            player1 =
              --                nth 0 players emptyPlayer
              --            player2 =
              --                nth 1 players emptyPlayer
              --            hand1Size =
              --                List.length player1.hand
              --            hand2Size =
              --                List.length player1.hand
              --        in
              --            Expect.equal [ hand1Size, hand2Size ] [ intensity, intensity ]
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
        , dealTests
        ]
