{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text (Text)
import Data.Time (Day)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Reader
import Control.Monad.Except


class EntityKey a where
  type Key a :: *

data Entity a = Entity {
  key :: Key a,
  value :: a
                       }

instance (ToJSON a, Key a ~ Int) => ToJSON (Entity a) where
  toJSON (Entity k v) = Object $ HashMap.insert "id" (toJSON k) m
     where m = case toJSON v of
             Object o -> o
             x -> HashMap.singleton "value" $ toJSON x

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data GroupMember = GroupMember
  {
    groupMemberFirstname :: Text,
    groupMemberLastname :: Text,
    groupMemberDob :: Day
  } deriving (Eq, Show)

instance EntityKey GroupMember where
  type Key GroupMember = Int

$(deriveJSON defaultOptions ''GroupMember)

data Group = Group {
  groupName :: Text,
  groupMembers :: [GroupMember]
                   } deriving (Show,Eq)

instance EntityKey Group where
  type Key Group = Int

$(deriveJSON defaultOptions ''Group)

data Project = Project {
  projectName :: Text,
  projectDescription :: Text
                       } deriving (Show, Eq)

instance EntityKey Project where
  type Key Project = Int

$(deriveJSON defaultOptions ''Project)

type CRUD a = "create" :> ReqBody '[JSON] a :> Post '[JSON] (Entity a)
              :<|> Capture "id" (Key a) :> DeleteNoContent '[JSON] NoContent
              :<|> Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] (Entity a)
              :<|> Capture "id" (Key a) :> Get '[JSON] (Entity a)


crudGroup :: ServeApp (CRUD Group)
crudGroup = (\x -> ask >>= \i -> return $ Entity i x)
  :<|> (\_ -> return NoContent)
  :<|> (\_ x -> return $ Entity 1 x)
  :<|> (\_ -> return $ Entity 1 $ Group "Group Name" [])

newtype App a = App {
  runApp :: ReaderT Int IO a
                    } deriving (Monad, Functor, Applicative, MonadReader Int, MonadIO)


type ServeApp a = ServerT a App

type API = "users" :> Get '[JSON] [User]
        :<|> "group" :> CRUD Group

runAppT :: Int -> App a -> ExceptT ServantErr IO a
runAppT i action = liftIO $ runReaderT (runApp action) i

testServer' :: Int -> Server API
testServer' code = enter (Nat (runAppT code)) server

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api (testServer' 5)

api :: Proxy API
api = Proxy

server :: ServeApp API
server = return users :<|> crudGroup

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
