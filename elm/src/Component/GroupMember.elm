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

type alias Model = {
        groupmember : GroupMember,
        jwtToken : String
    }

type Msg = UpdateFirstname String
         | UpdateLastname String
         | UpdateDob String
         | SaveGroupMember
         | SavedGroupMember (RemoteData.WebData GroupMember)
         | UpdateJwtToken String

view : Model -> Html Msg
view model = div [] [
              input [placeholder "First name", onInput UpdateFirstname] [],
              input [placeholder "Last name", onInput UpdateLastname] [],
              label [] [text "Date of birth"],
              node "input" [type_ "date", onInput UpdateDob] []
             ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  UpdateJwtToken t -> ({model | jwtToken = t}, Cmd.none)
  _ -> let (grpmember,cmd) = updateGroupMember msg model
       in ({model | groupmember = grpmember}, cmd)

updateGroupMember : Msg -> Model -> (GroupMember, Cmd Msg)
updateGroupMember msg model =
    let grpmember = model.groupmember
    in case msg of
      UpdateJwtToken _ -> (grpmember, Cmd.none)
      UpdateFirstname x -> ({grpmember | firstname = x}, Cmd.none)
      UpdateLastname x -> ({grpmember | lastname = x}, Cmd.none)
      UpdateDob x -> case fromString x of
        Ok d -> ({grpmember | dob = d}, Cmd.none)
        Err e -> (grpmember, Cmd.none)
      SaveGroupMember -> (grpmember, saveGroupMember model.jwtToken grpmember)
      SavedGroupMember RemoteData.NotAsked -> (grpmember, Cmd.none)
      SavedGroupMember RemoteData.Loading -> (grpmember, Cmd.none)
      SavedGroupMember (RemoteData.Failure _) -> (grpmember, Cmd.none)
      SavedGroupMember (RemoteData.Success g) -> (g, Cmd.none)

saveGroupMember jwt g = let (method, url) = case g.id of
                           Nothing -> ("POST", "http://localhost:8080/groupmember/create")
                           Just i -> ("PUT", "http://localhost:8080/groupmember/create" ++ toString i)
                    in Http.request {
                            method = method,
                            url = url,
                            body = Http.jsonBody <| encodeGroupMember g,
                            expect = Http.expectJson decodeGroupMember,
                            headers = [
                                 Http.header "authorization" jwt
                                ],
                            timeout = Nothing,
                            withCredentials = False
                        } |> Http.send (SavedGroupMember << RemoteData.fromResult)

startDate = dateFromFields 2000 (intToMonth 1) 1 0 0 0 0

model = Model (GroupMember "" "" startDate Nothing) ""

main = Html.program {
           init = (model,Cmd.none),
           view = view,
           update = update,
           subscriptions = \_ -> Sub.none
       }
