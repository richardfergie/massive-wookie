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

type alias Model = {
        projects : WebData (List (Entity Project)),
        jwtToken : Maybe String
    }

getProjects : Model -> Cmd Msg
getProjects model = Http.request {
                        method = "GET",
                        url = "http://localhost:8080/project",
                        body = Http.emptyBody,
                        expect = Http.expectJson (Decode.list (decodeEntity decodeProject)),
                        headers = [
                             Http.header "Content-Type" "application/json",
                             Http.header "authorization" (Maybe.withDefault "" model.jwtToken)
                            ],
                        timeout = Nothing,
                        withCredentials = False
                    } |> Http.send (ProjectResponse << RemoteData.fromResult)

init = (Model NotAsked Nothing, Cmd.none)

type Msg = LoadProjects
    | ProjectResponse (WebData (List (Entity Project)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  LoadProjects -> (model, getProjects model)
  ProjectResponse resp -> ({model | projects = resp}, Cmd.none)

view : Model -> Html Msg
view model = case (model.projects, model.jwtToken) of
  (_,Nothing) -> div [] [text "You must be logged in to see anything here"]
  (NotAsked,_) -> div [] [text "Starting to load projects..."]
  (Loading,_) -> div [] [text "Loading projects"]
  (Failure err,_) -> div [] [text "Error loading projects"]
  (Success proj,_) -> case proj of
     [] -> div [] [text "You have no projects"]
     ps -> div [] [text "You have some projects"]

main = Html.program {
           init = init,
           view = view,
           update = update,
           subscriptions = \_ -> Sub.none
       }
