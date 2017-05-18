module TestHelpers exposing (..)

import Types exposing (..)
import NonEmptyList as NEL exposing (..)
import GameState exposing (..)


emptyPlayer : Player
emptyPlayer =
    namedPlayer Felix Rain


namedPlayer : Grizzled -> Threat -> Player
namedPlayer name luckycharm =
    (Player (GrizzledCard name luckycharm) [] False [] Playing [] [])


felix : Player
felix =
    (Player (GrizzledCard Felix Rain) [] False [] Playing [] [])


lazare : Player
lazare =
    (Player (GrizzledCard Lazare Shell) [] True [] Playing [] [])


gustave : Player
gustave =
    (Player (GrizzledCard Gustave Winter) [] False [] Playing [] [])


twoPlayers : NonEmptyList Player
twoPlayers =
    NonEmptyList felix [ lazare ]


threePlayers : NonEmptyList Player
threePlayers =
    NonEmptyList felix [ lazare, gustave ]


threatCard threat =
    ThreatsCard (Card (NonEmptyList threat []) False)


game4Trial3MoraleCards : Game
game4Trial3MoraleCards =
    (Game
        threePlayers
        Nothing
        Nothing
        (List.repeat 4 (threatCard Rain))
        (List.repeat 3 (threatCard Winter))
        []
        [ Nothing, Nothing, Nothing, Nothing, Nothing ]
    )
