module Helpers.Types exposing (..)

import Html exposing (..)
import Dict exposing (Dict, fromList)
import List
import Generated.Types exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Task
import Time

type MessageType = Important
                 | Standard

type alias Message =
    {
        messageType : MessageType,
        messageBody : String,
        messageExpires : Time.Time
    }

generateMessage : MessageType -> String -> Float -> Cmd Message
generateMessage mtype mbody duration = Task.perform identity <| Task.map (\x -> Message mtype mbody (x+(Time.second * duration))) Time.now

type alias Form a =
    {
        result : a,
        message : Maybe String
    }

pureCommand x = Task.perform (\x->x) <| Task.succeed x

displayMessage f = case f.message of
  Nothing -> div [] []
  Just m -> div [] [ text m ]

getResult m = m.result

initialForm i = Form i Nothing

type alias Entity a = {
        entityKey : Int,
        entityVal : a
    }

entitiesToDict : List (Entity a) -> Dict Int a
entitiesToDict l = List.map (\x -> (x.entityKey,x.entityVal)) l |> Dict.fromList

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

type alias UserCreds = {
        userCredsId : Int,
        userCredsUsername : String,
        userCredsExpires : Float,
        userCredsIssued : Float
    }

decodeUserCreds : Decoder UserCreds
decodeUserCreds =
    decode UserCreds
        |> required "_userCredsId" int
        |> required "_userCredsUsername" string
        |> required "_userCredsExpires" float
        |> required "_userCredsIssued" float
