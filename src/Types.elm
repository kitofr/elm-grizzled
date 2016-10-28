module Types exposing (..)

import List exposing (..)
import Random exposing (..)


getThreats : ThreatList -> List Threat
getThreats list =
    list.head :: list.rest


nth n lst def =
    List.drop n lst |> List.head |> Maybe.withDefault def


type alias ThreatList =
    { head : Threat
    , rest : List Threat
    }


type ThreatsCard
    = Threats ThreatList
    | Trap ThreatList


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
    | Gas
    | Siren
    | Shell

type alias LuckyCharm = Threat
type alias Speach = Threat

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
    }
