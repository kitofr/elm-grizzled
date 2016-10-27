module State exposing (..)
import Types exposing(Model, Msg)

initialState : (Model, Cmd Msg)
initialState =  
    ({ game = {} }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
