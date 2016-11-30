module Component.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jwt
import Json.Encode as Encode
import Json.Decode exposing (string)
import Http

import Helpers.Types as Types

type alias Model =
    { username : String,
      password : String,
      message : String
    }

type alias LoginDetails = {
        usercreds : Types.UserCreds,
        jwtToken : String
    }

init = (Model "" "" "", Cmd.none)

type Msg = UpdateUsername String
         | UpdatePassword String
         | Login
         | LoginSuccess LoginDetails

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  UpdateUsername u -> ({model | username = u}, Cmd.none)
  UpdatePassword p -> ({model | password = p}, Cmd.none)
  Login -> ({model | message="Attempting login..."}, attemptLogin model.username model.password)
  LoginSuccess u -> ({model | message = "Login success"}, Cmd.none)

view : Model -> Html Msg
view model = div [] [
              div [] [text model.message],
              input [placeholder "Username", onInput UpdateUsername] [],
              input [placeholder "Password", onInput UpdatePassword] [],
              button [onClick Login] [text "Login"]
             ]

attemptLogin uname pword =
    let obj = Encode.object [ ("username", Encode.string uname),
                         ("password", Encode.string pword)
                       ]
    in Http.send validateLogin <| Http.post "http://localhost:8080/auth/login" (Http.jsonBody obj) string

validateLogin : Result (Http.Error) String -> Msg
validateLogin res = case res of
  Err (Http.BadUrl s) -> Debug.crash s
  Err Http.Timeout -> Debug.crash "Timeout"
  Err Http.NetworkError -> Debug.crash "Network Error"
  Err e -> Debug.crash "Other error"
  Ok s -> case Jwt.decodeToken Types.decodeUserCreds s of
     Err e -> Debug.crash "JWT error"
     Ok ucreds -> LoginSuccess <| LoginDetails ucreds s

main = Html.program {
           init = init,
           view = view,
           update = update,
           subscriptions = \_ -> Sub.none
       }
