module Tests exposing (..)

import Test exposing (..)
import List exposing (..)
import Expect
import String
import Types exposing (..)


all : Test
all =
    describe "Grizzled"
        [ describe "A threats card "
            [ test "has at least one threat" <|
                \() ->
                    let
                        card =
                            (Threats (ThreatList Rain []))
                    in
                        case card of
                            Threats list ->
                                let
                                    l =
                                        getThreats list

                                    head =
                                        nth 0 l Winter
                                in
                                    Expect.equal head Rain

                            _ ->
                                Expect.equal 1 2
            ]
        ]
