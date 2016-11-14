module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Style exposing (..)
import Random exposing (..)


rootView : Model -> Html Msg
rootView model =
    let
        seed =
            Random.initialSeed 1344
    in
        div []
            (List.map cardComponent (createDeck Nilfgaard seed))


cardComponent : Types.Card -> Html Msg
cardComponent card =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "margin-right", "5px" )
            ]
        ]
        [ div [ class "elm-grizzledCard" ]
            [ img
                [ src ((factionStr card) ++ ".png")
                , alt "Avatar"
                , style [ ( "width", "100%" ) ]
                ]
                []
            , div [ class "elm-grizzledContainer" ]
                [ h4 []
                    [ b [] [ text (factionStr card) ] ]
                , p [] [ text (combatTypeStr card) ]
                , p [] [ text (toString card.value) ]
                ]
            ]
        ]
