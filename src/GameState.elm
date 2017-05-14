module GameState exposing (..)

import Types exposing (..)
import NonEmptyList as NEL exposing (..)
import Util exposing (cycleBy, atLeast)


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


handleSupport : Game -> Game
handleSupport game =
    game


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
                            List.take cardsToDistribute game.trialsPile

                        trialsPile_ =
                            List.drop cardsToDistribute game.trialsPile

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
                                    , trialsPile = trialsPile_
                                    , players = p
                                }

                            Nothing ->
                                game

                _ ->
                    game

        _ ->
            game


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

        trialsPile_ =
            List.append game.trialsPile cards2Transfer
    in
        { game
            | trialsPile = trialsPile_
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


findPlayer : Game -> GrizzledCard -> Maybe Player
findPlayer game persona =
    game.players
        |> NEL.asList
        |> List.filter (\x -> x.persona.name == persona.name)
        |> List.head


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


changeMissionLeader : Game -> Game
changeMissionLeader game =
    let
        currentLeader =
            missionLeader game

        token =
            List.head game.speachTokens
    in
        case currentLeader of
            Just leader ->
                case token of
                    Just token ->
                        let
                            previousLeader =
                                { leader | speachTokens = token :: leader.speachTokens }
                        in
                            { game | players = (updatePlayer game.players previousLeader) }

                    _ ->
                        game

            _ ->
                game


missionLeader : Game -> Maybe Player
missionLeader game =
    game.players
        |> NEL.asList
        |> List.filter (\x -> x.missionLeader)
        |> List.head



--|> Maybe.withDefault emptyPlayer
