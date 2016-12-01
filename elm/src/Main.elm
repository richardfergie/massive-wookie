module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Component.Group as Group
import Component.Project as Project
import Component.Login as Login
import Component.Overview as Overview
import Helpers.Types as Types
import Maybe
import Dict
import Time

type View = GroupView
          | ProjectView
          | LoginView
          | OverviewView

type alias Model =
    {
        group : Maybe Group.Group,
        project : Maybe Project.Project,
        login : Login.Model,
        overview : Overview.Model,
        view : View,
        user : Maybe Login.LoginDetails,
        messages : List Types.Message
    }

model = Model Nothing Nothing (Login.Model "" "") (Tuple.first Overview.init) LoginView Nothing []

init = (model, Cmd.none)

groupSize g = case g of
  Nothing -> 0
  Just grp -> Dict.size grp.members

type Msg = UpdateGroup Group.Msg
         | UpdateProject Project.Msg
         | UpdateLogin Login.Msg
         | UpdateOverview Overview.Msg
         | ChangeView View
         | Logout
         | Tick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Tick t -> let oldmessages = model.messages
                newmessages = List.filter (\x -> x.messageExpires > t) oldmessages
            in ({model | messages = newmessages}, Cmd.none)
  Logout -> ({model | user = Nothing, view=LoginView}, Cmd.map (UpdateLogin << Login.Message) <| Types.generateMessage Types.Standard "Logged out" 3)
  UpdateGroup g -> let (newgroup, cmd) = Group.update g <| Maybe.withDefault Group.model model.group
                   in ({model | group = Just newgroup}, Cmd.map UpdateGroup cmd)
  UpdateProject p -> let newproject = Project.update p <| Maybe.withDefault Project.model model.project
                     in ({model | project = Just newproject},Cmd.none)
  UpdateLogin (Login.Message m) -> let oldmessages = model.messages
                                       newmessages = m :: model.messages
                                   in ({model | messages = newmessages}, Cmd.none)
  UpdateLogin (Login.LoginSuccess u) -> let (newlogin,msg) = Login.update (Login.LoginSuccess u) model.login
                                            overviewmodel = model.overview
                                            newoverview = {overviewmodel | jwtToken = Just u.jwtToken}
                                  in ({model | user = Just u, view=OverviewView, login=newlogin, overview=newoverview}, Cmd.batch [Cmd.map UpdateOverview <| Overview.getOverviewData newoverview,
                          Cmd.map (UpdateLogin << Login.Message) <| Types.generateMessage Types.Standard "Logged in" 3]
                                          )
  UpdateLogin l -> let (newlogin,msg) = Login.update l model.login
                   in ({model | login=newlogin}, Cmd.map UpdateLogin msg)
  UpdateOverview Overview.AddGroup -> ({model | view=GroupView}, Cmd.none)
  UpdateOverview Overview.AddProject -> ({model | view=ProjectView}, Cmd.none)
  UpdateOverview o -> let (newoverview,cmd) = Overview.update o model.overview
                      in ({model | overview = newoverview}, Cmd.map UpdateOverview cmd)
  ChangeView v -> case v of
    OverviewView -> ({model | view = OverviewView},Cmd.map UpdateOverview <| Overview.getOverviewData model.overview )
    _ -> ({model | view = v}, Cmd.none)

view : Model -> Html Msg
view m = div [] [
    navBar m,
    viewMessages m,
    case m.view of
      GroupView -> div [] [
                groupView m.group,
                case groupSize m.group of
                    0 -> button [] [text "Project ->"]
                    _ -> button [class "button-primary", onClick (ChangeView ProjectView)] [text "Project ->"]
                ]
      ProjectView -> div [] [
                  projectView m.project,
                  button [onClick (ChangeView GroupView)] [text "<- Group"]
                      ]
      LoginView -> Html.map UpdateLogin <| Login.view m.login
      OverviewView -> Html.map UpdateOverview <| Overview.view m.overview
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
                     li [] [a [onClick (ChangeView OverviewView)] [text "Overview"]],
                     li [] [a [] [text "Dummy"]],
                     li [] [a [onClick Logout] [text "Logout"]]
                         ]
                 ]



groupView g = Html.map UpdateGroup <| case g of
  Nothing -> Group.view Group.model
  Just grp -> Group.view grp

projectView p = Html.map UpdateProject <| case p of
  Nothing -> Project.view Project.model
  Just prj -> Project.view prj

main = Html.program {
           init = init,
           view = view,
           update = update,
           subscriptions = \_ -> Time.every (Time.second) (\t -> Tick t)
       }
