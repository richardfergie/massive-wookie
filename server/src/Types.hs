{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import GHC.Generics
import Data.Text (Text)
import Data.Time (Day)
import Data.Aeson
import Data.ByteString (ByteString)

data Entity a = Entity { entityKey :: Int,
                         entityVal :: a
                       } deriving (Generic, Show, ToJSON, FromJSON)

instance Eq (Entity a) where
  (==) x y = (==) (entityKey x) (entityKey y)

instance Ord (Entity a) where
  compare x y = compare (entityKey x) (entityKey y)

type FacilitatorId = Int
type GroupId = Int
type ProjectId = Int
type GroupMemberId = Int
type PanelId = Int
type CommunityPanelMemberId = Int
type OrganisationId = Int
type UserId = Int

data ProjectStatus = Created | Submitted | Validated | Granted deriving (Show, Generic,ToJSON,FromJSON)
data Project = Project { facilitator :: FacilitatorId,
                         group :: GroupId,
                         panel :: Maybe PanelId,
                         status :: ProjectStatus
                       } deriving (Show, Generic,ToJSON,FromJSON)

data GroupMember = GroupMember {
  firstname :: Text,
  lastname :: Text,
  dob :: Day
                               } deriving (Show, Generic, ToJSON, FromJSON)

data Group = Group { organisation :: OrganisationId,
                     members :: [GroupMemberId],
                     groupName :: Text
                   } deriving (Show, Generic, ToJSON, FromJSON)

data Panel = Panel { panelGroup :: GroupId,
                     panelFacilitator :: FacilitatorId,
                     otherPanelMember :: Either FacilitatorId CommunityPanelMemberId
                   } deriving (Show,Generic, ToJSON, FromJSON)

data CommunityPanelMember = CommunityPanelMember deriving (Show,Generic)

data Facilitator = Facilitator {
  facilitatorName :: Text,
  facilitatorUser :: UserId,
  facilitatorOrganisations :: [OrganisationId]
                               } deriving (Show, Generic, ToJSON, FromJSON)

data Organisation = Organisation {
  organisationName :: Text
                                 } deriving (Show, Generic, ToJSON, FromJSON)

type Username = Text

data User = User {
  username :: Username,
  hashedPassword :: ByteString
                 } deriving (Show,Eq,Generic)
