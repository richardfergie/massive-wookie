module Component.Group exposing (..)

import Component.GroupMember as GroupMember
import Helpers.Types as Types
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import List
import Maybe
import Generated.Types as Types
import Task
import Http
import RemoteData

type alias Group =
    {
        organisation : Int,
        members : Dict.Dict Int Types.GroupMember,
        groupName : String,
        id : Maybe Int
    }

type Msg = UpdateGroupName String
         | UpdateGroupMember Int GroupMember.Msg
         | AddGroupMember
         | RemoveGroupMember Int
         | SaveGroup
         | SetGroup (RemoteData.WebData Group)

viewGroupMembers : Dict.Dict Int Types.GroupMember -> List (Html Msg)
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

update : String -> Msg -> Group -> (Group, Cmd Msg)
update jwt msg model =
  let members = model.members
  in case msg of
    UpdateGroupName n -> ({model | groupName = n}, Cmd.none)
    AddGroupMember -> let k = Maybe.withDefault 1 <| List.maximum <| Dict.keys members
                      in ({model | members = Dict.insert (k+1) (Types.GroupMember "" "" GroupMember.startDate Nothing) members}, Cmd.none)
    RemoveGroupMember k -> ({ model | members = Dict.remove k members}, Cmd.none)
    UpdateGroupMember k m -> let r = Maybe.map (\x -> GroupMember.update jwt m x) (Dict.get k members)
      in case r of
        Nothing -> (model, Cmd.none)
        Just res -> ({model | members = Dict.insert k (Tuple.first res) members}, Cmd.none)
    SaveGroup -> (model, saveGroup jwt model)
    SetGroup (RemoteData.Success g) -> (g, Cmd.none)
    SetGroup _ -> (model, Cmd.none)

model = Group 1000 Dict.empty "" Nothing

-- save group needs to first save the members and only then save the group
-- Cmd.batch doesn't have any ordering, so must use Task instead
-- But task must also update the Groupmember once it is run??

saveGroupMemberHttp : String -> Types.GroupMember -> Task.Task Http.Error Types.GroupMember
saveGroupMemberHttp jwt g = let (method, url) = case g.id of
                           Nothing -> ("POST", "http://localhost:8080/groupmember/create")
                           Just i -> ("PUT", "http://localhost:8080/groupmember/" ++ toString i)
                    in Http.request {
                            method = method,
                            url = url,
                            body = Http.jsonBody <| Types.encodeGroupMember g,
                            expect = Http.expectJson Types.decodeGroupMember,
                            headers = [
                                 Http.header "authorization" jwt
                                ],
                            timeout = Nothing,
                            withCredentials = False
                        } |> Http.toTask

saveGroupHttp : String -> Group -> Task.Task Http.Error Types.Group
saveGroupHttp jwt grp =  let (method, url) = case grp.id of
                           Nothing -> ("POST", "http://localhost:8080/group/create")
                           Just i -> ("PUT", "http://localhost:8080/group/" ++ toString i)
                    in Http.request {
                            method = method,
                            url = url,
                            body = Http.jsonBody <| Types.encodeGroup <| toWireGroup grp,
                            expect = Http.expectJson Types.decodeGroup,
                            headers = [
                                 Http.header "authorization" jwt
                                ],
                            timeout = Nothing,
                            withCredentials = False
                        } |> Http.toTask

fromWireGroup : Dict.Dict Int Types.GroupMember -> Types.Group -> Group
fromWireGroup d g = Group g.organisation d g.groupName g.id

toWireGroup : Group -> Types.Group
toWireGroup g = Types.Group g.organisation (List.filterMap identity <| List.map (.id << Tuple.second) <| Dict.toList g.members) g.groupName g.id

saveGroupMembers jwt grp = Task.map Dict.fromList <| Task.map addNumbers <| Task.sequence <| List.map Tuple.second <| Dict.toList <| Dict.map (\_ x -> saveGroupMemberHttp jwt x) grp.members

saveGroupTask : String -> Group -> Task.Task Http.Error Group
saveGroupTask jwt grp = let dict = saveGroupMembers jwt grp
                            grouptask = Task.andThen (\x -> saveGroupHttp jwt {grp | members = x}) dict
                        in Task.map2 (\d grp -> fromWireGroup d grp) dict grouptask

addNumbers xs = let m = List.length xs
                    rng = List.range 1 m
                in List.map2 (\x y -> (x,y)) rng xs

saveGroup jwt grp = Task.attempt (RemoteData.fromResult >> SetGroup) <| saveGroupTask jwt grp
