module Tests exposing (..)

import UtilTests exposing (utilTests)
import NonEmptyListTests exposing (nonEmptyListTests)
import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList as NEL exposing (..)
import GameState exposing (..)


dealTests =
    describe "Dealing cards"
        [ test "Deal a list of cards to serveral players" <|
            \() ->
                [ felix, lazare ]
                    |> dealCards [ (threatCard Rain), (threatCard Winter), (threatCard Rain) ]
                    |> List.map .hand
                    |> Expect.equal [ [ (threatCard Rain), (threatCard Rain) ], [ (threatCard Winter) ] ]
        ]


emptyPlayer : Player
emptyPlayer =
    namedPlayer Felix Rain


namedPlayer : Grizzled -> Threat -> Player
namedPlayer name luckycharm =
    (Player (GrizzledCard name luckycharm) [] False [] Playing [] [])


felix : Player
felix =
    (Player (GrizzledCard Felix Rain) [] False [] Playing [] [])


lazare : Player
lazare =
    (Player (GrizzledCard Lazare Shell) [] True [] Playing [] [])


twoPlayers : NonEmptyList Player
twoPlayers =
    NonEmptyList felix [ lazare ]


threatCard threat =
    ThreatsCard (Card (NonEmptyList threat []) False)


game4Trial3MoraleCards : Game
game4Trial3MoraleCards =
    (Game
        twoPlayers
        InWar
        Nothing
        (List.repeat 4 (threatCard Rain))
        (List.repeat 3 (threatCard Winter))
        []
        [ Nothing, Nothing, Nothing, Nothing, Nothing ]
    )


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


missionTests =
    describe "Mission rules"
        [ describe "Preparation"
            [ test "A game starts with preparation " <|
                \() ->
                    let
                        game =
                            game4Trial3MoraleCards

                        intensity =
                            3

                        preparedGame =
                            preparation game intensity
                    in
                        Expect.equal preparedGame.missionState (Just (Preparation intensity))
            ]
        , describe "The mission"
            [ test "The mission starts with a prepared game" <|
                \() ->
                    let
                        inMissionGame =
                            preparation game4Trial3MoraleCards 4
                                |> enterMission
                    in
                        Expect.equal inMissionGame.missionState (Just TheMission)
            , test "entering a mission all players gets cards from the trails pile" <|
                \() ->
                    let
                        intensity =
                            2

                        inMissionGame =
                            preparation game4Trial3MoraleCards intensity
                                |> enterMission
                    in
                        Expect.equal (List.length inMissionGame.trailsPile) ((List.length game4Trial3MoraleCards.trailsPile) - intensity * 2)
            , test "hand size of players have increased with the mission intensitiy number of cards" <|
                \() ->
                    let
                        game =
                            preparation game4Trial3MoraleCards 2
                                |> enterMission

                        cardCountOnHand =
                            NEL.asList game.players
                                |> List.map (\p -> List.length p.hand)
                    in
                        Expect.equal cardCountOnHand [ 2, 2 ]
            , test "The mission ends when all players have withdrawn" <|
                \() ->
                    let
                        game =
                            preparation game4Trial3MoraleCards 2
                                |> enterMission
                                |> playTurn (Withdraw Left)
                                |> playTurn (Withdraw Right)
                    in
                        Expect.equal game.missionState (Just Support)
            , test "The mission continues until all is withdrawn" <|
                \() ->
                    let
                        game =
                            preparation game4Trial3MoraleCards 2
                                |> enterMission
                                |> playTurn (Withdraw Left)
                    in
                        Expect.equal game.missionState
                            (Just TheMission)
              -- support goes to the player that was given most support
              -- support can be a tie
            , test "morale drop is equal to the number of cards at hand but at least 3" <|
                \() ->
                    let
                        reserve =
                            List.length game4Trial3MoraleCards.moraleReserve

                        intensity =
                            1

                        game =
                            preparation game4Trial3MoraleCards intensity
                                |> enterMission
                                |> playTurn (Withdraw Left)
                                |> playTurn (Withdraw Right)
                                |> handleSupport
                                |> moraleDrop
                    in
                        Expect.equal (List.length game.moraleReserve) (reserve - 3)
            , describe "End of mission"
                [ test "before next mission, last leader gets a token" <|
                    \() ->
                        let
                            intensity =
                                2

                            game =
                                preparation game4Trial3MoraleCards intensity
                                    |> enterMission

                            currentLeader =
                                missionLeader game

                            numberOfTokens =
                                case currentLeader of
                                    Just leader ->
                                        List.length leader.speachTokens

                                    _ ->
                                        -1

                            afterGame =
                                game
                                    |> handleSupport
                                    |> moraleDrop
                                    |> changeMissionLeader

                            previousLeader =
                                case currentLeader of
                                    Just leader ->
                                        findPlayer afterGame leader.persona
                                            |> Maybe.withDefault emptyPlayer

                                    _ ->
                                        emptyPlayer
                        in
                            Expect.equal (List.length previousLeader.speachTokens) (numberOfTokens + 1)
                ]
            ]
        ]


all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        , missionTests
        , dealTests
        , utilTests
        , updatePlayerTests
        ]
