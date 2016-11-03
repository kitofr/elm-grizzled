module Types exposing (..)

import List exposing (..)
import Random exposing (..)
import NonEmptyList exposing (..)


getThreats : NonEmptyList Threat -> List Threat
getThreats list =
    asList list


nth : Int -> List a -> a -> a
nth n lst def =
    List.drop n lst |> List.head |> Maybe.withDefault def


type alias NoMansLand =
    { cards : List TrialCard }


type alias MoraleDrop =
    Int


type alias MissionIntensity =
    Int


type GameState
    = FlowerOfTheGun
    | NewMission MissionIntensity
    | MissionFailed MoraleDrop
    | MissionComplete MoraleDrop


type TurnEvent
    = Play TrialCard
    | Hold Speach
    | Turn LuckyCharm
    | Give SupportTile


type ThreatsCard
    = Threats (NonEmptyList Threat)
    | Trap (NonEmptyList Threat)


type TrialCard
    = Hardknock
    | ThreatsCard


type Card
    = TrialCard
    | PeaceCard
    | MonumentCard


type SupportTile
    = Left
    | Right
    | DoubleLeft
    | DoubleRight


type SpeechToken
    = Speach Threat


type Grizzled
    = Charles
    | Lazare
    | Gustave
    | Gaston
    | Felix
    | Anselme


type Threat
    = Rain
    | Winter
    | Night
    | Mask
    | Whistle
    | Shell


type alias LuckyCharm =
    Threat


type alias Speach =
    Threat


type alias GrizzledCard =
    { name : Grizzled
    , luckyCharm : Threat
    }


type alias MissionLeader =
    Bool


type alias Player =
    { persona : GrizzledCard
    , supportTiles : List SupportTile
    , missionLeader : Maybe MissionLeader
    , hand : List TrialCard
    , hardKnocks :
        List TrialCard
        -- This should be a list of most 3 and must be HardKnocks
    }
