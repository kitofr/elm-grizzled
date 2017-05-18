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


type alias PlayerList =
    -- This is not true, should be a list of one missionleader and at least 1 player
    NonEmptyList Player


type alias TrialsPile =
    List TrialCard


type alias MoraleReserve =
    List TrialCard


type MissionStep
    = Preparation -- MissionIntensity
    | TheMission
    | Success
    | Failure
    | Support
    | Victory
    | Defeat
    | MoraleDrop
    | AssignMissionLeader

type Event =
  Ready
    | AllWithdrawn
    | SomeFailed
    | Peace
    | FourOrMoreHardknocks
    | Monument

proceed : MissionStep -> Event -> MissionStep
proceed state event =
  case state of
    Preparation -> 
      case event of
        Ready -> TheMission
        _ -> state
    TheMission ->
      case event of
        AllWithdrawn -> Success
        SomeFailed -> Failure
        _ -> state
    Success -> Support
    Failure -> Support
    Support ->
      case event of
      Peace -> Victory
      FourOrMoreHardknocks -> Defeat
      _ -> MoraleDrop
    MoraleDrop ->
      case event of
        Monument -> Defeat
        _ -> AssignMissionLeader
    AssignMissionLeader -> Preparation
    Victory -> Victory
    Defeat -> Defeat

                              

type TurnAction
    = Play TrialCard
    | Use LuckyCharm
    | Make Speach
    | Withdraw SupportTile


type alias Trap =
    Bool


type alias Card =
    { threats : NonEmptyList Threat
    -- Maybe Trap?
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


type alias SpeachToken =
    Maybe Speach


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
    , luckyCharm : LuckyCharm
    }


type alias MissionLeader =
    Bool


type alias HardKnockList =
    List Hardknock


type alias Game =
    { players : PlayerList
    , missionState : Maybe MissionStep
    , missionIntensity : Maybe MissionIntensity
    , trialsPile : TrialsPile
    , moraleReserve : MoraleReserve
    , noMansLand : NoMansLand
    , speachTokens : List SpeachToken
    }


type PlayerState
    = Playing
    | Withdrawn SupportTile


type alias Player =
    { persona : GrizzledCard
    , supportTiles : List SupportTile
    -- This is wrong, only one player can be missionleader
    , missionLeader : MissionLeader
    , hand : List TrialCard
    , state : PlayerState
    -- This should be a list of most 3 and must be HardKnocks
    , hardKnocks :
        HardKnockList
    , speachTokens :
        List SpeachToken
    }
