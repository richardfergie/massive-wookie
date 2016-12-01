module Generated.Types exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import String
import Date exposing (..)
import Date.Extra.Config.Config_en_gb exposing (config)
import Date.Extra.Format as Format

type alias Organisation =
    { organisationName : String,
      id : Maybe Int
    }

decodeOrganisation : Decoder Organisation
decodeOrganisation =
    decode Organisation
        |> required "organisationName" string
        |> optional "id" (maybe int) Nothing

type alias GroupMember =
    { firstname : String
    , lastname : String
    , dob : Date
    , id : Maybe Int
    }

resultToDecoder : Result String a -> Json.Decode.Decoder a
resultToDecoder result =
  case result of
    Ok val ->
      Json.Decode.succeed val
    Err reason ->
      Json.Decode.fail reason

decodeGroupMember : Decoder GroupMember
decodeGroupMember =
    decode GroupMember
        |> required "firstname" string
        |> required "lastname" string
        |> required "dob" (string |> map Date.fromString |> andThen resultToDecoder )
        |> optional "id" (maybe int) Nothing

encodeGroupMember g = let lst = [
  ("firstname", Encode.string g.firstname),
  ("lastname", Encode.string g.lastname),
  ("dob", Encode.string <| Format.format config "%Y-%m-%d" g.dob)
                      ] ++ case g.id of
                               Nothing -> []
                               Just i -> [("id", Encode.int i)]
  in Encode.object lst

type alias Group =
    { organisation : Int
    , members : List (Int)
    , groupName : String
    , id : Maybe Int
    }

decodeGroup : Decoder Group
decodeGroup =
    decode Group
        |> required "organisation" int
        |> required "members" (list int)
        |> required "groupName" string
        |> optional "id" (maybe int) Nothing

encodeGroup g = let lst = [
  ("organisation", Encode.int g.organisation),
  ("groupName", Encode.string g.groupName),
  ("members", Encode.list <| List.map Encode.int g.members)
                    ] ++ case g.id of
                               Nothing -> []
                               Just i -> [("id", Encode.int i)]
  in Encode.object lst

type alias Facilitator =
    { facilitatorName : String
    , facilitatorUser : Int
    , facilitatorOrganisations : List (Int)
    , id : Maybe Int
    }

decodeFacilitator : Decoder Facilitator
decodeFacilitator =
    decode Facilitator
        |> required "facilitatorName" string
        |> required "facilitatorUser" int
        |> required "facilitatorOrganisations" (list int)
        |> optional "id" (maybe int) Nothing

type ProjectStatus
    = Created
    | Submitted
    | Validated
    | Granted

type alias Project =
    { projectName : String
    , projectDescription : String
    , facilitator : Int
    , group : Int
    , panel : Maybe (Int)
    , status : ProjectStatus
    , id : Maybe Int
    }
