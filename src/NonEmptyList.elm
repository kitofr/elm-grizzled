module NonEmptyList exposing (..)

import List exposing (..)


type alias NonEmptyList a =
    { head : a
    , rest : List a
    }


head : NonEmptyList a -> a
head nel =
    nel.head


asList : NonEmptyList a -> List a
asList nel =
    nel.head :: nel.rest


count : NonEmptyList a -> Int
count nel =
    List.length (asList nel)


add : a -> NonEmptyList a -> NonEmptyList a
add a nel =
    { nel | rest = List.append nel.rest [ a ] }


merge : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
merge a b =
    { head = b.head, rest = List.append b.rest (asList a) }
