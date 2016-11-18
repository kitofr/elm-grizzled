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
    { game | mission = Just (Preparation intensity) }


dealCards : List TrialCard -> List Player -> List Player
dealCards cards players =
    let
        hands =
            cycleBy
                (List.length players)
                cards
    in
        List.map2 (\p cs -> { p | hand = cs }) players hands


dealTests =
    describe "Dealing cards"
        [ test "Deal a list of cards to serveral players" <|
            \() ->
                [ namedPlayer Felix, namedPlayer Lazare ]
                    |> dealCards [ (threatCard Rain), (threatCard Winter), (threatCard Rain) ]
                    |> List.map .hand
                    |> Expect.equal [ [ (threatCard Rain), (threatCard Rain) ], [ (threatCard Winter) ] ]
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
                                    | mission = Just TheMission
                                    , trailsPile = trailsPile_
                                    , players = p
                                }

                            Nothing ->
                                game

                _ ->
                    game

        _ ->
            game


emptyPlayer : Player
emptyPlayer =
    namedPlayer Felix


namedPlayer : Grizzled -> Player
namedPlayer name =
    (Player (GrizzledCard name Rain) [] False [] [])


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
            , test "hand size of players have increased with the mission intensitiy number of cards" <|
                \() ->
                    let
                        intensity =
                            2

                        game =
                            preparation defaultGame intensity
                                |> enterMission

                        hands =
                            NEL.asList game.players
                                |> List.map (\p -> List.length p.hand)
                    in
                        Expect.equal hands [ intensity, intensity ]
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
        , utilTests
        ]
