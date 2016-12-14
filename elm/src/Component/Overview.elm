module Component.Overview exposing (..)

import RemoteData exposing (..)
import Generated.Types exposing (..)
import Helpers.Types exposing (..)
import Http
import Maybe
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Dict

type alias Model = {
        projects : WebData (List (Project)),
        groups : WebData (List (Group)),
        jwtToken : Maybe String
    }

getOverviewData : Model -> Cmd Msg
getOverviewData model = Cmd.batch [getGroups model, getProjects model]

getProjects : Model -> Cmd Msg
getProjects model = Http.request {
                        method = "GET",
                        url = "http://localhost:8080/project",
                        body = Http.emptyBody,
                        expect = Http.expectJson (Decode.list decodeProject),
                        headers = [
                             Http.header "Content-Type" "application/json",
                             Http.header "authorization" (Maybe.withDefault "" model.jwtToken)
                            ],
                        timeout = Nothing,
                        withCredentials = False
                    } |> Http.send (ProjectResponse << RemoteData.fromResult)

getGroups : Model -> Cmd Msg
getGroups model = Http.request {
                        method = "GET",
                        url = "http://localhost:8080/group",
                        body = Http.emptyBody,
                        expect = Http.expectJson (Decode.list decodeGroup),
                        headers = [
                             Http.header "Content-Type" "application/json",
                             Http.header "authorization" (Maybe.withDefault "" model.jwtToken)
                            ],
                        timeout = Nothing,
                        withCredentials = False
                    } |> Http.send (GroupResponse << RemoteData.fromResult)

init = (Model NotAsked NotAsked Nothing, Cmd.none)

type Msg = ProjectResponse (WebData (List Project))
         | GroupResponse (WebData (List Group))
         | AddGroup
         | AddProject
         | ChangeView View

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  ProjectResponse resp -> ({model | projects = resp}, Cmd.none)
  GroupResponse resp -> ({model | groups = resp}, Cmd.none)
  AddProject -> (model, Cmd.none)
  AddGroup -> (model, Cmd.none)
  ChangeView _ -> (model, Cmd.none)

view : Model -> Html Msg
view model = case model.jwtToken of
  Nothing -> div [] [text "You need to be logged in to view this"]
  Just _ -> div [] [
             div [] [h2 [] [text "Groups"],
                     viewGroups model
                    ],
             div [] [h2 [] [text "Projects"],
                     viewProjects model
                    ]
            ]

projectButton = div [] [
                 button [onClick AddProject] [text "New Project"]
                ]
groupButton = div [] [
               button [onClick AddGroup] [text "New Group"]
              ]

viewProjects : Model -> Html Msg
viewProjects model = case model.projects of
  NotAsked -> div [] [text "Starting to load projects..."]
  Loading -> div [] [text "Loading projects..."]
  Failure err -> div [] [text "Error loading projects"]
  Success proj -> case proj of
     [] -> div [] [text "You have no projects",
                   projectButton
                  ]
     ps -> div [] <| (List.map viewProject ps)
                   ++ [projectButton]


viewGroups : Model -> Html Msg
viewGroups model = case model.groups of
  NotAsked -> div [] [text "Starting to load groups..."]
  Loading -> div [] [text "Loading groups..."]
  Failure err -> div [] [text "Error loading groups"]
  Success grps -> case grps of
    [] -> div [] [text "You have no groups",
                  groupButton
                 ]
    gps -> div [] <| (List.map viewGroup gps)
                   ++ [groupButton]

viewGroup : Group -> Html Msg
viewGroup grp = div [] [
                 div [] [
                     a [onClick (ChangeView <| GroupView grp.id), href "#"] [text grp.groupName]
                     ]
                ]

viewProject : Project -> Html Msg
viewProject proj = div [] [
                    a [onClick <| ChangeView <| ProjectView proj.id, href "#"] [text proj.projectName]
                   ]

main = Html.program {
           init = init,
           view = view,
           update = update,
           subscriptions = \_ -> Sub.none
       }
