{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)

import qualified Types

instance ElmType Types.Organisation
instance ElmType Types.GroupMember
instance ElmType Types.Group
instance ElmType Types.Facilitator
instance ElmType Types.ProjectStatus
instance ElmType Types.Project

spec :: Spec
spec = Spec ["Generated", "Types"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Types.Organisation)
             : toElmDecoderSource (Proxy :: Proxy Types.Organisation)
             : toElmTypeSource    (Proxy :: Proxy Types.GroupMember)
             : toElmDecoderSource (Proxy :: Proxy Types.GroupMember)
             : toElmTypeSource    (Proxy :: Proxy Types.Group)
             : toElmDecoderSource (Proxy :: Proxy Types.Group)
             : toElmTypeSource    (Proxy :: Proxy Types.Facilitator)
             : toElmDecoderSource (Proxy :: Proxy Types.Facilitator)
             : toElmTypeSource    (Proxy :: Proxy Types.ProjectStatus)
          --   : toElmDecoderSource (Proxy :: Proxy Types.ProjectStatus)
             : toElmTypeSource    (Proxy :: Proxy Types.Project)
          --   : toElmDecoderSource (Proxy :: Proxy Types.Project)
             : []
             )


main = do
  putStrLn "Generating Elm"
  specsToDir [spec] "../elm/src/"
