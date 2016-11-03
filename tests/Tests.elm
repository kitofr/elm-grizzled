module Tests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)
import NonEmptyList exposing (..)


nonEmptyListTests =
    describe "Non empty list"
    [ test "has at least one item" <|
        \() ->
            let
                list =
                    NonEmptyList "a" []
            in
                Expect.equal list.head "a"
    , test "asList creates a list" <|
        \() ->
            let
                list =
                    NonEmptyList "a" [ "b" ]
            in
                Expect.equal (asList list) [ "a", "b" ]
    ]

all : Test
all =
    describe "Grizzled"
        [ nonEmptyListTests
        ]
