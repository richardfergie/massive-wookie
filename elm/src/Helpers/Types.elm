module Helpers.Types exposing (..)

import Html exposing (..)
import Dict
import Generated.Types exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

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

type alias Entity a = {
        entityKey : Int,
        entityVal : a
    }

decodeEntity : Decoder a -> Decoder (Entity a)
decodeEntity dec =
    decode Entity
        |> required "entityKey" int
        |> required "entityVal" dec


stringToProjectStatus : String -> Result String ProjectStatus
stringToProjectStatus x = case x of
  "Created" -> Ok Created
  "Submitted" -> Ok Submitted
  "Validated" -> Ok Validated
  "Granted" -> Ok Granted
  _ -> Err "Unknown project status"

decodeProjectStatus : Decoder ProjectStatus
decodeProjectStatus = string
                    |> Json.Decode.map stringToProjectStatus
                    |> andThen resultToDecoder


decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "projectName" string
        |> required "projectDescription" string
        |> required "facilitator" int
        |> required "group" int
        |> required "project" (nullable int)
        |> required "status" decodeProjectStatus
