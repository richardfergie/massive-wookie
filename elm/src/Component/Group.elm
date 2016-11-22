module Component.Group exposing (..)

import Component.GroupMember as GroupMember
import Helpers.Types as Types
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import List
import Maybe

type alias Group =
    {
        name : String,
        members : Dict.Dict Int GroupMember.GroupMember
    }

type Msg = UpdateGroupName String
         | UpdateGroupMember Int GroupMember.Msg
         | AddGroupMember
         | RemoveGroupMember Int

viewGroupMembers : Dict.Dict Int GroupMember.GroupMember -> List (Html Msg)
viewGroupMembers d = Dict.foldl (\k v acc -> acc ++ [Html.map (UpdateGroupMember k) (GroupMember.view (Types.initialForm v)), button [onClick <| RemoveGroupMember k] [text "Remove"]]) [] d

view : Group -> Html Msg
view model = div [] <| [input [placeholder "Group name", onInput UpdateGroupName] [],
                        div [] [
                             span [] [text <| toString <| Dict.size model.members],
                             text " members"
                             ],
                        div [] (viewGroupMembers (model.members)),
                        div [] [
                             button [onClick AddGroupMember] [text "Add group member"]
                            ]
                       ]

update : Msg -> Group -> Group
update msg model =
  let members = model.members
  in case msg of
    UpdateGroupName n -> {model | name = n}
    AddGroupMember -> let k = Maybe.withDefault 1 <| List.maximum <| Dict.keys members
                      in {model | members = Dict.insert (k+1) (GroupMember.GroupMember "" "" GroupMember.startDate) members}
    RemoveGroupMember k -> { model | members = Dict.remove k members}
    UpdateGroupMember k m -> let r = Maybe.map (\x -> Types.getResult <| GroupMember.update m <| Types.initialForm x) (Dict.get k members)
      in case r of
        Nothing -> model
        Just res -> {model | members = Dict.insert k res members}

model = Group "" Dict.empty
