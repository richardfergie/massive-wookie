{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text (Text)
import Data.Time (Day)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Acid
import Data.Acid.Local
import Control.Monad.Trans.Either
import Control.Exception (bracket)

import qualified Crud
import qualified Acid
import qualified Types

type CRUD a = "create" :> ReqBody '[JSON] a :> Post '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> DeleteNoContent '[JSON] NoContent
               :<|> Capture "id" Int :> ReqBody '[JSON] a :> Put '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> Get '[JSON] (Maybe (Types.Entity a))

crudGroup :: ServerT (CRUD Types.Group) Crud.CRUD
crudGroup = (\g -> Crud.createGroup g)
             :<|> (\i -> Crud.deleteGroup i >> return NoContent)
             :<|> (\i b -> Crud.setGroup i b)
             :<|> (\i -> Crud.getGroup i)

crudGroupMember :: ServerT (CRUD Types.GroupMember) Crud.CRUD
crudGroupMember = Crud.createGroupMember
               :<|> (\i -> Crud.deleteGroupMember i >> return NoContent)
               :<|> Crud.setGroupMember
               :<|> Crud.getGroupMember

toHandler :: AcidState Acid.World -> Crud.CRUD a -> Handler a
toHandler acid c = do
  res <- liftIO $ runExceptT $ Acid.acidStateCrud acid c
  ExceptT $ return $ case res of
    Right x -> Right x
    Left (Crud.ForeignKeyMissing s) -> Left err400
    Left (Crud.NotFound) -> Left err404

handlerServer :: AcidState Acid.World -> Server API
handlerServer acid = enter (Nat $ toHandler acid) $ (crudGroupMember :<|> crudGroup)

api :: Proxy API
api = Proxy

app :: AcidState Acid.World -> Application
app acid = serve api (handlerServer acid)

startApp :: IO ()
startApp = bracket (openLocalStateFrom "/tmp/acid" Acid.emptyWorld)
                   (\acid -> closeAcidState acid)
                   (\acid -> run 8080 (app acid))

type API = "groupmember" :> CRUD Types.GroupMember
        :<|> "group" :> CRUD Types.Group
