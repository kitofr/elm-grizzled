module NonEmptyList exposing (..)

import List exposing (..)


type alias NonEmptyList a =
    { head : a
    , rest : List a
    }


fromList : List a -> Maybe (NonEmptyList a)
fromList list =
    case list of
        [ h ] ->
            Just (NonEmptyList h [])

        [ a, b ] ->
            Just (NonEmptyList a [ b ])

        h :: t ->
            Just (NonEmptyList h t)

        [] ->
            Nothing


drop : Int -> NonEmptyList a -> Maybe (NonEmptyList a)
drop n list =
    list
        |> asList
        |> List.drop n
        |> fromList


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
