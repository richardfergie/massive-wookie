module Component.Group exposing (..)

import Component.GroupMember as GroupMember
import Helpers.Types as Types
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import List
import Maybe
import Generated.Types exposing (..)
import Task

type alias Group =
    {
        name : String,
        members : Dict.Dict Int GroupMember.Model,
        jwtToken : String
    }

type Msg = UpdateGroupName String
         | UpdateGroupMember Int GroupMember.Msg
         | AddGroupMember
         | RemoveGroupMember Int
         | SaveGroup
         | UpdateJwtToken String

viewGroupMembers : Dict.Dict Int GroupMember.Model -> List (Html Msg)
viewGroupMembers d = Dict.foldl (\k v acc -> acc ++ [Html.map (UpdateGroupMember k) (GroupMember.view v), button [onClick <| RemoveGroupMember k] [text "Remove"]]) [] d

view : Group -> Html Msg
view model = div [] <| [input [placeholder "Group name", onInput UpdateGroupName] [],
                        div [] [
                             span [] [text <| toString <| Dict.size model.members],
                             text " members"
                             ],
                        div [] (viewGroupMembers (model.members)),
                        div [] [
                             button [onClick AddGroupMember] [text "Add group member"],
                             button [onClick SaveGroup] [text "Save Group"]
                            ]
                       ]

passJwtTokenDown : String -> Group -> Cmd Msg
passJwtTokenDown jwt grp = let members = grp.members
                           in Cmd.batch <|
                               List.map Tuple.second <|
                                   Dict.toList <|
                                       Dict.map (\i _ -> Cmd.map (UpdateGroupMember i) <|
                                                     Task.perform identity <|
                                                     Task.succeed <| GroupMember.UpdateJwtToken jwt) members

update : Msg -> Group -> (Group, Cmd Msg)
update msg model =
  let members = model.members
  in case msg of
    UpdateJwtToken s -> ({model | jwtToken = s}, passJwtTokenDown s model)
    UpdateGroupName n -> ({model | name = n}, Cmd.none)
    AddGroupMember -> let k = Maybe.withDefault 1 <| List.maximum <| Dict.keys members
                      in ({model | members = Dict.insert (k+1) (GroupMember.Model (GroupMember "" "" GroupMember.startDate Nothing) model.jwtToken) members}, Cmd.none)
    RemoveGroupMember k -> ({ model | members = Dict.remove k members}, Cmd.none)
    UpdateGroupMember k m -> let r = Maybe.map (\x -> GroupMember.update m x) (Dict.get k members)
      in case r of
        Nothing -> (model, Cmd.none)
        Just res -> ({model | members = Dict.insert k (Tuple.first res) members}, Cmd.none)
    SaveGroup -> (model, saveGroupG members)

saveGroupG g = Cmd.batch <| List.map Tuple.second <| Dict.toList <| Dict.map (\i m -> Cmd.map (UpdateGroupMember i) <| GroupMember.saveGroupMember m.jwtToken m.groupmember) g

model = Group "" Dict.empty ""
