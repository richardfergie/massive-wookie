module Generated.Types exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String
import Date exposing (..)


type alias Organisation =
    { organisationName : String
    }

decodeOrganisation : Decoder Organisation
decodeOrganisation =
    decode Organisation
        |> required "organisationName" string

type alias GroupMember =
    { firstname : String
    , lastname : String
    , dob : Date
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

type alias Group =
    { organisation : Int
    , members : List (Int)
    , groupName : String
    }

decodeGroup : Decoder Group
decodeGroup =
    decode Group
        |> required "organisation" int
        |> required "members" (list int)
        |> required "groupName" string

type alias Facilitator =
    { facilitatorName : String
    , facilitatorUser : Int
    , facilitatorOrganisations : List (Int)
    }

decodeFacilitator : Decoder Facilitator
decodeFacilitator =
    decode Facilitator
        |> required "facilitatorName" string
        |> required "facilitatorUser" int
        |> required "facilitatorOrganisations" (list int)

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
    , status :ProjectStatus
    }
