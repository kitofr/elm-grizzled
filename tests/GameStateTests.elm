module GameStateTests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList as NEL exposing (..)
import GameState exposing (..)
import TestHelpers exposing (..)

type State = S0 | S1 | S2
type Event = A | B

accepts : State -> (List Event) -> Bool
accepts state action =
  case state of
    S0 -> case action of
      A :: xs -> accepts S1 xs
      B :: xs -> accepts S2 xs
      _ -> False
    S1 -> case action of
      A :: xs -> accepts S2 xs
      B :: xs -> accepts S0 xs
      _ -> False
    S2 -> case action of
      A :: xs -> accepts S0 xs
      B :: xs -> accepts S2 xs
      _ -> True

decide : (List Event) -> Bool
decide = accepts S0


missionTests =
    describe "Mission rules"
        [ describe "State machine"
          [
            test "[A,A,A] should be False" <|
              \() ->
                Expect.equal (decide [A, A, A]) False
          , test "[A,A,A,A,B,B,A,A,A] should be True" <|
              \() ->
                Expect.equal (decide [A,A,A,A,B,B,A,A,A]) True
          , test "[B,A,B,A,B,A] should be False" <|
              \() ->
                Expect.equal (decide [B,A,B,A,B,A]) False
          ]
        , describe "Preparation"
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
                        Expect.equal preparedGame.missionState (Just (Preparation))
            ]
        , describe "The mission"
            [ test "starts with a prepared game" <|
                \() ->
                    let
                        inMissionGame =
                            preparation game4Trial3MoraleCards 4
                                |> enterMission
                    in
                        Expect.equal inMissionGame.missionState (Just TheMission)
            , test "entering a mission all players gets cards from the trials pile" <|
                \() ->
                    let
                        intensity =
                            2

                        inMissionGame =
                            preparation game4Trial3MoraleCards intensity
                                |> enterMission
                    in
                        Expect.equal (List.length inMissionGame.trialsPile) ((List.length game4Trial3MoraleCards.trialsPile) - intensity * 2)
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
                        Expect.equal cardCountOnHand [ 2, 1, 1 ]
            , test "The mission ends when all players have withdrawn" <|
                \() ->
                    let
                        game =
                            preparation game4Trial3MoraleCards 2
                                |> enterMission
                                |> playTurn (Withdraw Left)
                                |> playTurn (Withdraw Right)
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
--            , test "support goes to the player that was given most support" <|
--                \() ->
--                    let
--                        game =
--                            preparation game4Trial3MoraleCards 2
--                                |> enterMission
--                                |> playTurn (Withdraw Left)
--                                |> playTurn (Withdraw Right)
--                                |> playTurn (Withdraw Right)
--                    in
--                        Expect.equal game.missionState (Just Support)
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
