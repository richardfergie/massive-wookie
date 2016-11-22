module Helpers.Types exposing (..)

import Html exposing (..)
import Dict

type alias Form a =
    {
        result : a,
        message : Maybe String
    }

displayMessage f = case f.message of
  Nothing -> div [] []
  Just m -> div [] [ text m ]

getResult m = m.result

initialForm i = Form i Nothing
