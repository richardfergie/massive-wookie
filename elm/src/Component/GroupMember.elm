module Component.GroupMember exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (..)
import Helpers.Types exposing (..)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Core exposing (intToMonth)

type alias GroupMember =
    {
        firstname : String,
        lastname : String,
        dob : Date
    }

type Msg = UpdateFirstname String
         | UpdateLastname String
         | UpdateDob String

type alias Model = {
        groupmember : Form GroupMember
    }

view : Form GroupMember -> Html Msg
view model = div [] [
              displayMessage model,
              input [placeholder "First name", onInput UpdateFirstname] [],
              input [placeholder "Last name", onInput UpdateLastname] [],
              label [] [text "Date of birth"],
              node "input" [type_ "date", onInput UpdateDob] []
             ]

update : Msg -> Form GroupMember -> Form GroupMember
update msg model =
    let result = model.result
    in case msg of
      UpdateFirstname x -> {model | result = { result | firstname = x}}
      UpdateLastname x -> {model | result = {result | lastname = x}}
      UpdateDob x -> case fromString x of
        Ok d -> {model | result = {result | dob = d}}
        Err e -> {model | message = Just e}

startDate = dateFromFields 2000 (intToMonth 1) 1 0 0 0 0

model = Form (GroupMember "" "" startDate) Nothing

main = Html.beginnerProgram {
           model = model,
           view = view,
           update = update
       }
