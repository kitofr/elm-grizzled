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


drop : Int -> NonEmptyList a -> NonEmptyList a
drop n list =
    list
        |> asList
        |> List.drop n
        |> fromList
        |> Maybe.withDefault list


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


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map fn list =
    let
        head =
            fn list.head

        rest =
            List.map fn list.rest
    in
        NonEmptyList head rest


{-| Returns True if all elements satisfies the predicate
    >>> all (\x -> x == 1) (NonEmptyList 1 [])
    True
    >>> all (\x -> x == 1) (NonEmptyList 0 [])
    False
    >>> all (\x -> x == 1) (NonEmptyList 0 [1])
    False
    >>> all (\x -> x == 1) (NonEmptyList 1 [1])
    True
-}
all : (a -> Bool) -> NonEmptyList a -> Bool
all fn list =
    not (any (not << fn) list)


{-| Returns True if any elements satisfies the predicate
    >>> any (\x -> x == 1) (NonEmptyList 1 [])
    True
    >>> any (\x -> x == 1) (NonEmptyList 0 [])
    False
    >>> any (\x -> x == 1) (NonEmptyList 0 [1])
    True
-}
any : (a -> Bool) -> NonEmptyList a -> Bool
any fn list =
    if fn list.head then
        True
    else
        case list.rest of
            [] ->
                False

            x :: xs ->
                any fn (NonEmptyList x xs)
