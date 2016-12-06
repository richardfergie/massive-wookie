module Component.GroupMember exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (..)
import Helpers.Types exposing (..)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Core exposing (intToMonth)
import Generated.Types exposing (..)
import Helpers.Types as Types
import Http
import RemoteData

type alias Model = GroupMember

type Msg = UpdateFirstname String
         | UpdateLastname String
         | UpdateDob String
  --       | SaveGroupMember
  --       | SavedGroupMember (RemoteData.WebData GroupMember)

view : GroupMember -> Html Msg
view model = div [] [
              input [placeholder "First name", onInput UpdateFirstname] [],
              input [placeholder "Last name", onInput UpdateLastname] [],
              label [] [text "Date of birth"],
              node "input" [type_ "date", onInput UpdateDob] []
             ]

update : String -> Msg -> GroupMember -> (Model, Cmd Msg)
update jwt msg model = updateGroupMember jwt msg model

updateGroupMember : String -> Msg -> GroupMember -> (GroupMember, Cmd Msg)
updateGroupMember jwt msg model =
    case msg of
      UpdateFirstname x -> ({model | firstname = x}, Cmd.none)
      UpdateLastname x -> ({model | lastname = x}, Cmd.none)
      UpdateDob x -> case fromString x of
        Ok d -> ({model | dob = d}, Cmd.none)
        Err e -> (model, Cmd.none)

startDate = dateFromFields 2000 (intToMonth 1) 1 0 0 0 0

model = GroupMember "" "" startDate Nothing
