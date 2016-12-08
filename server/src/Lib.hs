{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Trans.Resource
import Data.UUID.V4
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified Crud
import qualified Acid
import qualified Types
import API
import Foundation
import Auth hiding (appServerNat, app, startApp)

appServerNat :: AppConfig -> Maybe UnverifiedJwtToken -> AppServer :~> Handler
appServerNat appconfig munverifiedtoken = Nat $ \action -> do
  rqid <- liftIO $ nextRandom
  mcreds <- verifyTokenFromHeader munverifiedtoken appconfig
  runResourceT $ runReaderT (runAppServer action) $ RequestInfo appconfig mcreds rqid

handlerServer :: AppConfig -> Server (AuthAPI API)
handlerServer appconfig munverifiedtoken = enter (appServerNat appconfig munverifiedtoken) (apiServer :<|> loginServer)


api :: Proxy (AuthAPI API)
api = Proxy

app :: AppConfig -> Application
app acid = serve api (handlerServer acid)

adminUser = Types.User "admin"
                       "sha256|20|q34s4hOsw5rbr7XUR1C9xQ==|sopSFIYk/EpafmgOG7jt88VDGtrB2BJg+iORj2gdUBo=" --"secret"

createAdmin = do
  muser <- Crud.getUser 0
  case muser of
    Nothing -> Crud.createUser adminUser >> return ()
    Just _ -> return ()

createTestData = do
  createAdmin
  Crud.setOrganisation 0 $ Types.Organisation "Test Organisation"
  Crud.setOrganisation 1 $ Types.Organisation "Another Test Org"
  Crud.setFacilitator 0 $ Types.Facilitator "Test Facilitator" 0 [0,1]

corsPolicy = const $ Just simpleCorsResourcePolicy{corsRequestHeaders = simpleResponseHeaders ++ ["Content-Type", "authorization"], corsMethods = "PUT" : "DELETE" : simpleMethods}

startApp :: IO ()
startApp = bracket (openLocalStateFrom "/tmp/acid" Acid.emptyWorld >>= \acid -> return $ AppConfig acid (parseJwk "secret"))
                   (\appconf -> closeAcidState $ Foundation.state appconf)
                   (\appconf -> do
                       runExceptT $ runResourceT $ runReaderT (runAppServer $ runDB createTestData) (RequestInfo appconf undefined undefined)
                       run 8080 (logStdoutDev $ cors corsPolicy $ app appconf))
