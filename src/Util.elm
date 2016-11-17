module Util exposing (..)

import List exposing (indexedMap, filterMap, map, range)


cycleBy : Int -> List a -> List (List a)
cycleBy n list =
    let
        get index col =
            List.indexedMap
                (\i x ->
                    if i % n == index then
                        Just x
                    else
                        Nothing
                )
                col
                |> List.filterMap identity
    in
        List.map (\i -> get (i) list)
            (List.range 0 (n - 1))
