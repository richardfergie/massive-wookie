{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation where

import Data.Acid
import Data.Text(Text)
import Data.Time.Clock.POSIX(POSIXTime)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Servant
import qualified Jose.Jwk as Jose
import Data.UUID

import Types
import Crud
import qualified Acid

data AppConfig = AppConfig {
  state :: AcidState Acid.World,
  jwtKey :: Jose.Jwk
                     }

data RequestInfo = RequestInfo {
  requestInfoAppConfig :: AppConfig,
  requestInfoUserCreds :: Maybe UserCreds,
  requestInfoId :: UUID
                                }

data UserCreds = UserCreds {
  _userCredsId :: UserId,
  _userCredsExpires :: POSIXTime,
  _userCredsIssued :: POSIXTime
                           } deriving (Show,Eq)

newtype AppServer a = AppServer {
  runAppServer :: ReaderT RequestInfo (ResourceT Handler) a
                                  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader RequestInfo, MonadError ServantErr)

runDB :: Crud.CRUDT AppServer a -> AppServer a
runDB x = do
  acid <- fmap (state . requestInfoAppConfig) ask
  res <- Acid.acidStateCrud acid x
  case res of
    Right r -> return r
    Left (ForeignKeyMissing s) -> throwError err400
    Left (NonUnique s) -> throwError err400
