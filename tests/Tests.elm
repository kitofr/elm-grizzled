module Tests exposing (..)

import UtilTests exposing (utilTests)
import NonEmptyListTests exposing (nonEmptyListTests)
import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList as NEL exposing (..)
import Util exposing (cycleBy)


preparation : Game -> MissionIntensity -> Game
preparation game intensity =
    { game | missionState = Just (Preparation intensity) }


dealCards : List TrialCard -> List Player -> List Player
dealCards cards players =
    let
        hands =
            cycleBy
                (List.length players)
                cards
    in
        List.map2 (\player cards -> { player | hand = cards }) players hands


dealTests =
    describe "Dealing cards"
        [ test "Deal a list of cards to serveral players" <|
            \() ->
                [ felix, lazare ]
                    |> dealCards [ (threatCard Rain), (threatCard Winter), (threatCard Rain) ]
                    |> List.map .hand
                    |> Expect.equal [ [ (threatCard Rain), (threatCard Rain) ], [ (threatCard Winter) ] ]
        ]


enterMission : Game -> Game
enterMission game =
    case game.missionState of
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

                        trailsPile_ =
                            List.drop cardsToDistribute game.trailsPile

                        players_ =
                            game.players
                                |> NEL.asList
                                |> dealCards newCards
                                |> NEL.fromList
                    in
                        case players_ of
                            Just p ->
                                { game
                                    | missionState = Just TheMission
                                    , trailsPile = trailsPile_
                                    , players = p
                                }

                            Nothing ->
                                game

                _ ->
                    game

        _ ->
            game


handleSupport : Game -> Game
handleSupport game =
    game


atLeast : Int -> Int -> Int
atLeast min a =
    if a < min then
        min
    else
        a


moraleDrop : Game -> Game
moraleDrop game =
    let
        cardsOnHand =
            NEL.asList game.players
                |> List.map (\x -> List.length x.hand)
                |> List.sum
                |> atLeast 3

        cards2Transfer =
            List.take cardsOnHand game.moraleReserve

        moralePile_ =
            List.drop cardsOnHand game.moraleReserve

        trailsPile_ =
            List.append game.trailsPile cards2Transfer
    in
        { game
            | trailsPile = trailsPile_
            , moraleReserve = moralePile_
        }


withdrawPlayer : SupportTile -> Player -> Player
withdrawPlayer direction player =
    { player | state = Withdrawn direction }


playTurn : TurnAction -> Game -> Game
playTurn action game =
    case action of
        Withdraw direction ->
            let
                player_ =
                    game.players.head
                        |> withdrawPlayer direction

                rest =
                    NEL.drop 1 game.players

                players_ =
                    NEL.add player_ rest

                allWithdrawn =
                    NEL.asList players_
                        |> List.all
                            (\x ->
                                case x.state of
                                    Withdrawn _ ->
                                        True

                                    _ ->
                                        False
                            )
            in
                if allWithdrawn then
                    { game
                        | players = players_
                        , missionState = Just Support
                    }
                else
                    { game
                        | players = players_
                    }

        _ ->
            game


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


findPlayer : Game -> GrizzledCard -> Maybe Player
findPlayer game persona =
    game.players
        |> NEL.asList
        |> List.filter (\x -> x.persona.name == persona.name)
        |> List.head


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


updatePlayer : PlayerList -> Player -> PlayerList
updatePlayer list player =
    NEL.map
        (\item ->
            if item.persona.name == player.persona.name then
                player
            else
                item
        )
        list


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


changeMissionLeader : Game -> Game
changeMissionLeader game =
    let
        currentLeader =
            missionLeader game

        token =
            List.head game.speachTokens
    in
        case token of
            Just token ->
                let
                    previousLeader =
                        { currentLeader | speachTokens = token :: currentLeader.speachTokens }
                in
                    { game | players = (updatePlayer game.players previousLeader) }

            _ ->
                game


missionLeader : Game -> Player
missionLeader game =
    game.players
        |> NEL.asList
        |> List.filter (\x -> x.missionLeader)
        |> List.head
        |> Maybe.withDefault emptyPlayer


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
                                List.length currentLeader.speachTokens

                            afterGame =
                                game
                                    |> handleSupport
                                    |> moraleDrop
                                    |> changeMissionLeader

                            previousLeader =
                                findPlayer afterGame currentLeader.persona
                                    |> Maybe.withDefault emptyPlayer
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
