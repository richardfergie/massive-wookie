module Component.Project exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Project = {
        name : String,
        description : String,
        group : Int,
        id : Maybe Int
    }

type Msg = UpdateName String
         | UpdateDescription String
         | UpdateGroup Int

update msg model = case msg of
  UpdateName n -> {model | name = n}
  UpdateDescription d -> {model | description = d}
  UpdateGroup d -> {model | group = d}

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
