module Component.GroupMember exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (..)
import Helpers.Types exposing (..)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Core exposing (intToMonth)
import Date.Extra.Config.Config_en_gb exposing (config)
import Date.Extra.Format exposing (format)
import Generated.Types exposing (..)
import Helpers.Types as Types
import Http
import RemoteData

type alias Model = GroupMember

type Msg = UpdateFirstname String
         | UpdateLastname String
         | UpdateDob String

view : GroupMember -> Html Msg
view model = div [] [
              input [placeholder "First name", onInput UpdateFirstname, value model.firstname] [],
              input [placeholder "Last name", onInput UpdateLastname, value model.lastname] [],
              label [] [text "Date of birth"],
              node "input" [type_ "date", onInput UpdateDob, value <| format config "%Y-%m-%d" model.dob] []
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

loadGroupMemberHttp jwt i =
    Http.request {
            method = "GET",
            url = "http://localhost:8080/groupmember/" ++ toString i,
            body = Http.emptyBody,
            expect = Http.expectJson decodeGroupMember,
            headers = [
                 Http.header "authorization" jwt
            ],
            timeout = Nothing,
            withCredentials = False
        } |> Http.toTask
