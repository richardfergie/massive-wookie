module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Component.Group as Group
import Component.Project as Project
import Component.Login as Login
import Component.Overview as Overview
import Helpers.Types as Types
import Generated.Types as Types
import Maybe
import Dict
import Time
import Task
import RemoteData
import Http
import Json.Decode as Decode

type alias Model =
    {
        group : Maybe Group.Group,
        project : Maybe Project.Project,
        login : Login.Model,
        overview : Overview.Model,
        view : Types.View,
        user : Maybe Login.LoginDetails,
        facilitatorOrganisations : Maybe (Dict.Dict Int Types.Organisation),
        messages : List Types.Message
    }

model = Model Nothing Nothing (Login.Model "" "") (Tuple.first Overview.init) Types.LoginView Nothing Nothing []

init = (model, Cmd.none)

type Msg = UpdateGroup Group.Msg
         | UpdateProject Project.Msg
         | UpdateLogin Login.Msg
         | UpdateOverview Overview.Msg
         | UpdateOrganisations (Maybe (Dict.Dict Int Types.Organisation))
         | ChangeView Types.View
         | Logout
         | Tick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick t -> let oldmessages = model.messages
                newmessages = List.filter (\x -> x.messageExpires > t) oldmessages
            in ({model | messages = newmessages}, Cmd.none)
  Logout -> ({model | user = Nothing, view=Types.LoginView}, Cmd.map (UpdateLogin << Login.Message) <| Types.generateMessage Types.Standard "Logged out" 3)
  UpdateGroup (Group.ChangeView v) -> ({model | view=v},Cmd.none)
  UpdateGroup g -> let jwt = Maybe.withDefault "" <| Maybe.map .jwtToken model.user
                       orgid = Maybe.withDefault 0 <| List.head <| Dict.keys <| Maybe.withDefault Dict.empty model.facilitatorOrganisations
                       (newgroup, cmd) = Group.update jwt model.facilitatorOrganisations g <| Maybe.withDefault (Group.Group orgid Dict.empty "" Nothing) model.group
                   in ({model | group = Just newgroup}, Cmd.map UpdateGroup cmd)
  UpdateProject p -> let newproject = Project.update p <| Maybe.withDefault Project.model model.project
                     in ({model | project = Just newproject},Cmd.none)
  UpdateLogin (Login.Message m) -> let oldmessages = model.messages
                                       newmessages = m :: model.messages
                                   in ({model | messages = newmessages}, Cmd.none)
  UpdateLogin (Login.LoginSuccess u) -> let (newlogin,msg) = Login.update (Login.LoginSuccess u) model.login
                                            overviewmodel = model.overview
                                            newoverview = {overviewmodel | jwtToken = Just u.jwtToken}
                                  in ({model | user = Just u, view=Types.OverviewView, login=newlogin, overview=newoverview}, Cmd.batch [Cmd.map UpdateOverview <| Overview.getOverviewData newoverview,
                          Cmd.map (UpdateLogin << Login.Message) <| Types.generateMessage Types.Standard "Logged in" 3,
                                             getFacilitatorOrganisations u.jwtToken]
                                          )
  UpdateLogin l -> let (newlogin,msg) = Login.update l model.login
                   in ({model | login=newlogin}, Cmd.map UpdateLogin msg)
  UpdateOverview Overview.AddGroup -> ({model | view=Types.GroupView Nothing, group=Nothing}, Cmd.none)
  UpdateOverview Overview.AddProject -> ({model | view=Types.ProjectView}, Cmd.none)
  UpdateOverview (Overview.ChangeView v) ->
      let jwt = Maybe.withDefault "" <| Maybe.map .jwtToken model.user
      in case v of
          Types.GroupView (Just i) -> ({model | view=v}, Cmd.map UpdateGroup <| Group.loadGroupCmd jwt i)
          _ -> ({model | view=v}, Cmd.none)

  UpdateOverview o -> let (newoverview,cmd) = Overview.update o model.overview
                      in ({model | overview = newoverview}, Cmd.map UpdateOverview cmd)
  UpdateOrganisations Nothing -> (model, Cmd.none)
  UpdateOrganisations f -> ({model | facilitatorOrganisations=f},Cmd.none)
  ChangeView v -> case v of
    Types.OverviewView -> ({model | view = Types.OverviewView},Cmd.map UpdateOverview <| Overview.getOverviewData model.overview )
    Types.GroupView Nothing -> ({model | view = Types.GroupView Nothing}, Cmd.none)
    Types.GroupView (Just i) -> let jwt = Debug.log "change view" <| Maybe.withDefault "" <| Maybe.map .jwtToken model.user
                          in ({model | view = Types.GroupView (Just i)}, Cmd.map UpdateGroup (Group.loadGroupCmd jwt i))
    _ -> ({model | view = v}, Cmd.none)

view : Model -> Html Msg
view m = div [] [
    navBar m,
    viewMessages m,
    case m.view of
      Types.GroupView _ -> div [] [
                groupView m.facilitatorOrganisations m.group
                ]
      Types.ProjectView -> div [] [
                  projectView m.project,
                  button [onClick (ChangeView (Types.GroupView Nothing))] [text "<- Group"]
                      ]
      Types.LoginView -> Html.map UpdateLogin <| Login.view m.login
      Types.OverviewView -> Html.map UpdateOverview <| Overview.view m.overview
     ]

viewMessages : Model -> Html Msg
viewMessages model = div [] <| List.map (\x -> div [] [text x.messageBody]) model.messages

navBar : Model -> Html Msg
navBar model = case model.user of
  Nothing -> nav [class "primary"] [
    ul [] [
      li [] [
           a [] [text "Login"]
               ]
        ]
             ]
  Just _ -> nav [class "primary"] [
             ul [] [
                     li [] [a [onClick (ChangeView Types.OverviewView)] [text "Overview"]],
                     li [] [a [] [text "Dummy"]],
                     li [] [a [onClick Logout] [text "Logout"]]
                         ]
                 ]



groupView orgs g = let orgid = Maybe.withDefault 0 <| List.head <| Dict.keys <| Maybe.withDefault Dict.empty orgs
                   in Html.map UpdateGroup <| case g of
                        Nothing -> Group.view orgs <| Group.Group orgid Dict.empty "" Nothing
                        Just grp -> Group.view orgs grp

projectView p = Html.map UpdateProject <| case p of
  Nothing -> Project.view Project.model
  Just prj -> Project.view prj

idListToDict ls = let foo x = case x.id of
                                  Nothing -> Nothing
                                  Just i -> Just (i, x)
                  in Dict.fromList <| List.filterMap foo ls

getFacilitatorOrganisations jwt =
    Http.request {
            method="GET",
            url="http://localhost:8080/organisation",
            body = Http.emptyBody,
            expect = Http.expectJson <| Decode.list Types.decodeOrganisation,
            headers = [
                 Http.header "authorization" jwt
                ],
            timeout = Nothing,
            withCredentials = False
        } |> Http.toTask
          |> Task.map idListToDict
          |> Task.map (UpdateOrganisations << Just)
          |> Task.attempt (\res -> case res of
                                       Err e -> UpdateOrganisations Nothing
                                       Ok o -> o
                          )

main = Html.program {
           init = init,
           view = view,
           update = update,
           subscriptions = \_ -> Time.every (Time.second) (\t -> Tick t)
       }
