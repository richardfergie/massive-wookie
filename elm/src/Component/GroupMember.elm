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

type Msg = UpdateFirstname String
         | UpdateLastname String
         | UpdateDob String

view : GroupMember -> Html Msg
view model = div [] [
              input [placeholder "First name", onInput UpdateFirstname] [],
              input [placeholder "Last name", onInput UpdateLastname] [],
              label [] [text "Date of birth"],
              node "input" [type_ "date", onInput UpdateDob] []
             ]

update : Msg -> GroupMember -> GroupMember
update msg model =
    case msg of
      UpdateFirstname x -> {model | firstname = x}
      UpdateLastname x -> {model | lastname = x}
      UpdateDob x -> case fromString x of
        Ok d -> {model | dob = d}
        Err e -> model

startDate = dateFromFields 2000 (intToMonth 1) 1 0 0 0 0

model = GroupMember "" "" startDate

main = Html.beginnerProgram {
           model = model,
           view = view,
           update = update
       }
