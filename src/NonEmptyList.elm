module NonEmptyList exposing (..)

import List exposing (..)

type alias NonEmptyList a =
  { head : a
  , rest : List a
  }

asList : (NonEmptyList a) -> List a
asList nonEmpty =
  nonEmpty.head :: nonEmpty.rest

count : (NonEmptyList a) -> Int
count nonEmpty = 
  List.length (asList nonEmpty)
