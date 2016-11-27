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
import Control.Monad.Trans.Resource

import qualified Crud
import qualified Acid
import qualified Types
import API
import Foundation
import Auth hiding (appServerNat, app, AuthAPI)


appServerNat :: AppConfig -> Maybe (UnverifiedJwtToken) -> AppServer :~> Handler
appServerNat appconfig munverifiedtoken = Nat $ \action -> do
  rqid <- liftIO $ return 5
  mcreds <- verifyTokenFromHeader munverifiedtoken appconfig
  runResourceT $ runReaderT (runAppServer action) $ RequestInfo appconfig mcreds rqid

type AuthAPI = JwtAuthHeader :> API

handlerServer :: AppConfig -> Server (AuthAPI)
handlerServer appconfig munverifiedtoken = enter (appServerNat appconfig munverifiedtoken) (apiServer)


api :: Proxy AuthAPI
api = Proxy

app :: AppConfig -> Application
app acid = serve api (handlerServer acid)

startApp :: IO ()
startApp = bracket (openLocalStateFrom "/tmp/acid" Acid.emptyWorld >>= \acid -> return $ AppConfig acid (parseJwk "secret"))
                   (\appconf -> closeAcidState $ Foundation.state appconf)
                   (\acid -> run 8080 (app acid))
