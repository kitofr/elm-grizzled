module Grizzled exposing (main)

import Html
import State
import View


main =
    Html.program
        { init = State.initialState
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.rootView
        }
