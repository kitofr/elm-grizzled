module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (img, div, h4, p)
import Css.Namespace exposing (namespace)

type CssClasses
  = Card
  | Container

css =
  (stylesheet << namespace "elm-grizzled")
  [
    h4 [
      fontFamily fantasy
    ]
    ,(.) Card
    [ width (px 200) 
    , boxShadow5 (px 0) (px 4) (px 8) (px 0) (rgba 0 0 0 0.2)
    --, transition (s 0.3)
    , hover [
        boxShadow5 (px 0) (px 8) (px 16) (px 0) (rgba 0 0 0 0.2)
      ]
    ]
    ,(.) Container
    [ 
      padding2 (px 2) (px 16)   
    ]
  ]

