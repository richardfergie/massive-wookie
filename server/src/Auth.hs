{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGe TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Auth where

import Servant
import Control.Lens
import Data.Time.Clock.POSIX(POSIXTime,getPOSIXTime)
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Jose.Jwt as Jose
import qualified Jose.Jwe as Jose
import qualified Jose.Jwa as Jose
import qualified Jose.Jwk as Jose
import Control.Monad.IO.Class
import Data.ByteString.Lazy(toStrict,fromStrict)
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import qualified Data.ByteString.Base64 as Base64
import Network.Wai.Handler.Warp(run)
import Data.UUID.V4
import Crypto.PasswordStore
import Data.ByteString(ByteString)

import Types
import Foundation
import Acid
import qualified Crud
import TH

parseJwk :: Text -> Jose.Jwk
parseJwk secretStr = Jose.SymmetricJwk (encodeUtf8 secretStr) Nothing Nothing (Just $ Jose.Signed Jose.HS256)

$(deriveJSON defaultOptions ''UserCreds)

newtype UnverifiedJwtToken = UnverifiedJwtToken Text
  deriving (Show,Eq,FromHttpApiData)

$(deriveJSON defaultOptions ''UnverifiedJwtToken)

type JwtAuthHeader = Header "authorization" UnverifiedJwtToken

data LoginCreds = LoginCreds {
  username :: Text,
  password :: Text
                             } deriving Generic

$(deriveJSON defaultOptions ''LoginCreds)

createToken
  :: (MonadError ServantErr m, MonadIO m) =>
     UserId -> Username -> AppConfig -> m UnverifiedJwtToken
createToken uid uname aconf = do
  now <- liftIO getPOSIXTime
  let expires = now + 60*60*24 --one day
      token = UserCreds uid uname expires now
  mjwt <- liftIO $ Jose.encode [jwtKey aconf] (Jose.JwsEncoding Jose.HS256) (Jose.Claims $ toStrict $ encode token)
  case mjwt of
    Left _ -> throwError err500 {errBody = "Unable to authenticate"}
    Right jwt -> return $ UnverifiedJwtToken $ decodeUtf8 $ Jose.unJwt jwt

verifyToken
  :: (MonadError ServantErr m, MonadIO m) =>
     UnverifiedJwtToken -> AppConfig -> m UserCreds
verifyToken (UnverifiedJwtToken unverifiedText) aconf = do
  mjwtContent <- liftIO $ Jose.decode [jwtKey aconf] (Just $ Jose.JwsEncoding Jose.HS256) $ encodeUtf8 unverifiedText
  jwt <- case mjwtContent of
    Right (Jose.Jws (_, jwt)) -> return jwt
    _ -> throwError err401 { errBody = "Invalid token" }
  case eitherDecode (fromStrict jwt) of
        Left _ -> throwError err401 { errBody = "Token does not match expected format" }
        Right token -> do
            now <- liftIO getPOSIXTime
            case (_userCredsIssued token < now, _userCredsExpires token > now) of
              (False,_) -> throwError err401 { errBody = "Token not yet active"}
              (_,False) -> throwError err404 { errBody = "Token has expired" }
              (True, True) -> return token

verifyTokenFromHeader :: (MonadError ServantErr m, MonadIO m) =>
  (Maybe UnverifiedJwtToken) -> AppConfig -> m (Maybe UserCreds)
verifyTokenFromHeader Nothing _ = return Nothing
verifyTokenFromHeader (Just (UnverifiedJwtToken x)) aconf = do
    let unverifiedToken = if "Bearer " `Text.isPrefixOf` x then Text.drop 7 x else x
    t <- verifyToken (UnverifiedJwtToken unverifiedToken) aconf
    return $ Just t

appServerNat :: AppConfig -> Maybe (UnverifiedJwtToken) -> AppServer :~> Handler
appServerNat appconfig munverifiedtoken = Nat $ \action -> do
  rqid <- liftIO $ nextRandom
  mcreds <- verifyTokenFromHeader munverifiedtoken appconfig
  runResourceT $ runReaderT (runAppServer action) $ RequestInfo appconfig mcreds rqid

type AuthAPI a = JwtAuthHeader :> (a
             :<|> "auth" :> AuthRoutes)

type TestAPI  = AuthAPI (Get '[JSON] [Int])


requireUser :: AppServer UserCreds
requireUser = do
  mcreds <- fmap requestInfoUserCreds ask
  case mcreds of
    Nothing -> throwError $ err401 { errBody = "No or Bad Authorization header in request" }
    Just creds -> return creds

t :: AppServer [Int]
t = do
  mcreds <- fmap requestInfoUserCreds ask
  rqid <- fmap requestInfoId ask
  liftIO $ print rqid
  case mcreds of
    Nothing -> return []
    Just creds -> return [_userCredsId creds]

tokenStuff :: ServerT (Get '[JSON] UnverifiedJwtToken) AppServer
tokenStuff = do
  appconfig <- fmap requestInfoAppConfig ask
  createToken 1 "username" appconfig

type AuthRoutes = "login" :> ReqBody '[JSON] LoginCreds :> Post '[JSON] UnverifiedJwtToken
                :<|> "change-password" :> QueryParam "password" Text :> Post '[JSON] NoContent

login :: LoginCreds -> AppServer UnverifiedJwtToken
login (LoginCreds u unhashedpasswordtxt) = do
  let unhashedpassword = encodeUtf8 unhashedpasswordtxt
  muser <- runDB $ Crud.getUserByUsername u
  case muser of
    Nothing -> throwError err401{errBody="Username or password error"}
    Just (Entity uid (User uname hashedpassword)) -> case verifyPassword unhashedpassword hashedpassword of
      False -> throwError err401{errBody="Username or password error"}
      True -> do
        appconfig <- fmap requestInfoAppConfig ask
        createToken uid uname appconfig

changePassword :: Maybe Text -> AppServer NoContent
changePassword munhashedpasswordtxt = do
  let munhashedpassword = fmap encodeUtf8 munhashedpasswordtxt
  user <- requireUser
  case munhashedpassword of
    Nothing -> throwError err400{errBody="Must have password to change password"}
    Just unhashedpassword -> do
      hashedpassword <- liftIO $ makePassword unhashedpassword 20
      runDB $ Crud.setUser (_userCredsId user) (Types.User (_userCredsUsername user) hashedpassword)
      return NoContent

loginServer :: ServerT AuthRoutes AppServer
loginServer = login :<|> changePassword
{-
h :: AppConfig -> Server (TestAPI)
h conf unv = enter (appServerNat conf unv) (t :<|> tokenStuff)

proxy :: Proxy TestAPI
proxy = Proxy

app :: AppConfig -> Application
app key = serve proxy (h key)

startApp :: IO ()
startApp = run 8080 $ app (AppConfig undefined $ parseJwk "secret")
-}
