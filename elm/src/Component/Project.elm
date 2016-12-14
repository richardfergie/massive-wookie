module Component.Project exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData
import Generated.Types as Types
import Helpers.Types as Types
import Http
import Task
{-
type alias Project = {
        projectName : String,
        projectDescription : String,
        group : Int,
        status : Types.ProjectStatus,
        id : Maybe Int
    }
-}
type Msg = UpdateName String
         | UpdateDescription String
         | UpdateGroup Int
         | SaveProject
         | SetProject (RemoteData.WebData Types.Project)
         | LoadProject Int
         | ChangeView Types.View

update jwt msg model = case msg of
  UpdateName n -> ({model | projectName = n},Cmd.none)
  UpdateDescription d -> ({model | projectDescription = d},Cmd.none)
  UpdateGroup d -> ({model | group = d},Cmd.none)
  SaveProject -> (model, saveProject jwt model)
  SetProject (RemoteData.Success p) -> (p, Cmd.none)
  SetProject _ -> (model, Cmd.none)
  LoadProject i -> (model, loadProject jwt i)
  ChangeView v -> (model, Cmd.none)


view model = div [] [
              div [] [
              input [placeholder "Project name", onInput UpdateName, value model.projectName] []
                  ],
              div [] [
              textarea [attribute "rows" "20",
                        attribute "cols" "60",
                        placeholder "Project description",
                        onInput UpdateDescription,
                        value model.projectDescription
                       ] []
                  ],
              button [onClick SaveProject] [text "Save Project"]
             ]

model = Types.Project "" "" 0 Types.Created Nothing

saveProjectHttp : String -> Types.Project -> Task.Task Http.Error Types.Project
saveProjectHttp jwt proj = let (method, url) = case proj.id of
  Nothing -> ("POST", "http://localhost:8080/project/create")
  Just i -> ("PUT", "http://localhost:8080/project/" ++ toString i)
            in Http.request {
                    method = method,
                    url = url,
                    body = Http.jsonBody <| Types.encodeProject proj,
                    expect = Http.expectJson <| Types.decodeProject,
                    headers = [
                         Http.header "authorization" jwt
                        ],
                    timeout = Nothing,
                    withCredentials = False
                } |> Http.toTask

saveProject jwt proj = Task.attempt (RemoteData.fromResult >> SetProject) <| saveProjectHttp jwt proj

loadProjectHttp jwt i =
    Http.request {
            method = "GET",
            url = "http://localhost:8080/project/" ++ toString i,
            body = Http.emptyBody,
            expect = Http.expectJson Types.decodeProject,
            headers = [
                 Http.header "authorization" jwt
                ],
            timeout = Nothing,
            withCredentials = False
        } |> Http.toTask

loadProject jwt i = Task.attempt (RemoteData.fromResult >> SetProject) <| loadProjectHttp jwt i
