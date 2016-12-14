module Component.Project exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData
import Generated.Types as Types
import Helpers.Types as Types

type alias Project = {
        name : String,
        description : String,
        group : Int,
        id : Maybe Int
    }

type Msg = UpdateName String
         | UpdateDescription String
         | UpdateGroup Int
         | SaveProject
         | SetProject (RemoteData.WebData Project)
         | LoadProject Int
         | ChangeView Types.View

update msg model = case msg of
  UpdateName n -> ({model | name = n},Cmd.none)
  UpdateDescription d -> ({model | description = d},Cmd.none)
  UpdateGroup d -> ({model | group = d},Cmd.none)
  SaveProject -> (model,Cmd.none)
  SetProject p -> (model, Cmd.none)
  LoadProject i -> (model, Cmd.none)
  ChangeView v -> (model, Cmd.none)


view model = div [] [
              div [] [
              input [placeholder "Project name", onInput UpdateName] []
                  ],
              div [] [
              textarea [attribute "rows" "20",
                        attribute "cols" "60",
                        placeholder "Project description", onInput UpdateDescription] []
                  ]
             ]

model = Project "" "" 0 Nothing
