module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Component.Group as Group
import Component.Project as Project
import Component.Login as Login
import Helpers.Types as Types
import Maybe
import Dict

type View = GroupView
          | ProjectView
          | LoginView

type alias Model =
    {
        group : Maybe Group.Group,
        project : Maybe Project.Project,
        login : Login.Model,
        view : View,
        user : Maybe Types.UserCreds
    }

model = Model Nothing Nothing (Login.Model "" "" "") LoginView Nothing

init = (model, Cmd.none)

groupSize g = case g of
  Nothing -> 0
  Just grp -> Dict.size grp.members

type Msg = UpdateGroup Group.Msg
         | UpdateProject Project.Msg
         | UpdateLogin Login.Msg
         | ChangeView View

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  UpdateGroup g -> let newgroup = Group.update g <| Maybe.withDefault Group.model model.group
                   in ({model | group = Just newgroup}, Cmd.none)
  UpdateProject p -> let newproject = Project.update p <| Maybe.withDefault Project.model model.project
                     in ({model | project = Just newproject},Cmd.none)
  UpdateLogin (Login.LoginSuccess u) -> let (newlogin,msg) = Login.update (Login.LoginSuccess u) model.login
                                  in ({model | user = Just u, view = GroupView, login=newlogin}, Cmd.map UpdateLogin msg)
  UpdateLogin l -> let (newlogin,msg) = Login.update l model.login
                   in ({model | login=newlogin}, Cmd.map UpdateLogin msg)
  ChangeView v -> ({model | view = v}, Cmd.none)

view : Model -> Html Msg
view m = case m.view of
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
           subscriptions = \_ -> Sub.none
       }
