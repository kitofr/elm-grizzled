module Turn exposing (..)

import Types exposing (TrialCard, Speach, LuckyCharm)


type TurnEvent
    = Play TrialCard
    | Give Speach
    | Turn LuckyCharm
    | Pass
