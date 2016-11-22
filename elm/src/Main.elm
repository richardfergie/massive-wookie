module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Component.Group as Group
import Component.Project as Project
import Maybe
import Dict

type View = GroupView
          | ProjectView

type alias Model =
    {
        group : Maybe Group.Group,
        project : Maybe Project.Project,
        view : View
    }

model = Model Nothing Nothing GroupView

groupSize g = case g of
  Nothing -> 0
  Just grp -> Dict.size grp.members

type Msg = UpdateGroup Group.Msg
         | UpdateProject Project.Msg
         | ChangeView View

update : Msg -> Model -> Model
update msg model = case msg of
  UpdateGroup g -> let newgroup = Group.update g <| Maybe.withDefault Group.model model.group
                   in {model | group = Just newgroup}
  UpdateProject p -> let newproject = Project.update p <| Maybe.withDefault Project.model model.project
                     in {model | project = Just newproject}
  ChangeView v -> {model | view = v}

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

groupView g = Html.map UpdateGroup <| case g of
  Nothing -> Group.view Group.model
  Just grp -> Group.view grp

projectView p = Html.map UpdateProject <| case p of
  Nothing -> Project.view Project.model
  Just prj -> Project.view prj

main = Html.beginnerProgram {
           model = model,
           view = view,
           update = update
       }
