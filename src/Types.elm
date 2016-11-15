module Types exposing (..)

import List exposing (..)
import Random exposing (..)
import NonEmptyList exposing (..)


type alias Model =
    {}


type Msg
    = Foo


getThreats : NonEmptyList Threat -> List Threat
getThreats list =
    asList list


nth : Int -> List a -> a -> a
nth n lst def =
    List.drop n lst |> List.head |> Maybe.withDefault def


type alias NoMansLand =
    List TrialCard


type alias RemainingCardsInHand =
    Int


type alias MissionIntensity =
    Int


type alias SupportList =
    List { player : Player, supportTile : SupportTile }


type alias PlayerList =
    NonEmptyList Player


type alias TrialsPile =
    List TrialCard


type alias MoralePile =
    List TrialCard


type MissionStep
    = Preparation MissionIntensity
    | TheMission
    | Support SupportList
    | MoraleDrop RemainingCardsInHand


type TurnAction
    = Play TrialCard
    | Use LuckyCharm
    | Make Speach
    | Withdraw SupportTile


type alias Trap =
    Bool


type alias Card =
    { threats : NonEmptyList Threat
    , trap : Trap
    }


type Hardknock
    = Phobia Threat
    | Trauma Threat
    | Mute


type TrialCard
    = Hardknock
    | ThreatsCard Card
    | MerryChristmas


type SupportTile
    = Left
    | Right
    | DoubleLeft
    | DoubleRight


type SpeachToken
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


type alias HardKnockList =
    List Hardknock


type GameState
    = InWar
    | Peace
    | Monument


type alias Game =
    { players : PlayerList
    , state : GameState
    , mission : Maybe MissionStep
    , trailsPile : TrialsPile
    , moralePile : MoralePile
    , noMansLand : NoMansLand
    , speachTokens : List SpeachToken
    }


type alias Player =
    { persona : GrizzledCard
    , supportTiles : List SupportTile
    , missionLeader : MissionLeader
    , hand : List TrialCard
    , hardKnocks :
        HardKnockList
        -- This should be a list of most 3 and must be HardKnocks
    }
