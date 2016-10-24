{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Types where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

data Entity a = Entity { entityKey :: Key a,
                         entityVal :: a
                       }

deriving instance (Show a,Show (Key a)) => Show (Entity a)

type FacilitatorId = Int
type GroupId = Int
type ProjectId = Int
type GroupMemberId = Int
type PanelId = Int
type CommunityPanelMemberId = Int
type OrganisationId = Int

data ProjectStatus = Created | Submitted | Validated | Granted deriving (Show)
data Project = Project { facilitator :: FacilitatorId,
                         group :: GroupId,
                         panel :: Maybe PanelId,
                         status :: ProjectStatus
                       } deriving (Show)

data GroupMember = GroupMember deriving Show

data Group = Group { organisation :: OrganisationId,
                     members :: [GroupMemberId]
                   } deriving (Show)

data Panel = Panel { panelGroup :: GroupId,
                     panelFacilitator :: FacilitatorId,
                     otherPanelMember :: Either FacilitatorId CommunityPanelMemberId
                   } deriving Show

data CommunityPanelMember = CommunityPanelMember deriving Show

data Facilitator = Facilitator

class EntityKey a where
  type Key a :: *

instance EntityKey Project where
  type Key Project = ProjectId

instance EntityKey Group where
  type Key Group = GroupId

instance EntityKey Panel where
  type Key Panel = PanelId

instance EntityKey GroupMember where
  type Key GroupMember = GroupMemberId

instance EntityKey Facilitator where
  type Key Facilitator = FacilitatorId

data Table a = Table { _maxKey :: Int,
                       _tableMap :: Map Int a
                     } deriving (Show)
makeLenses ''Table

emptyTable = Table 0 Map.empty

tableInsert :: (Key a ~ Int) => a -> State (Table a) (Entity a)
tableInsert v = do
  Table k m <- get
  put $ Table (k+1) $ Map.insert k v m
  return $ Entity k v

tableLookup :: (Key a ~ Int) => Key a -> State (Table a) (Maybe (Entity a))
tableLookup k = do
  Table _ m <- get
  case Map.lookup k m of
    Nothing -> return Nothing
    Just v -> return $ Just $ Entity k v


data World = World { _worldProjects :: Table Project,
                     _worldGroups :: Table Group,
                     _worldPanels :: Table Panel,
                     _worldGroupMembers :: Table GroupMember
                   } deriving (Show)
makeLenses ''World

emptyWorld :: World
emptyWorld = World emptyTable emptyTable emptyTable emptyTable

insertWorld :: (Key a ~ Int, MonadState World m) => Lens' World (Table a) -> a -> m (Entity a)
insertWorld l p = do
  world <- get
  let k = world ^. l . maxKey
      m = world ^. l . tableMap
      newm = Map.insert k p m
      newtable = Table (k+1) newm
      newworld = world & l .~ newtable
  put newworld
  return $ Entity k p

updateWorld :: (Key a ~ Int, MonadState World m) => Lens' World (Table a) -> Key a -> a -> m (Entity a)
updateWorld l k v = do
  world <- get
  let m = world ^. l . tableMap
      newm = Map.insert k v m
      newworld = world & l . tableMap .~ newm
  put newworld
  return $ Entity k v

getWorld :: (Key a ~ Int, MonadState World m) => Getter World (Table a) -> Key a -> m (Maybe a)
getWorld getter k = do
  world <- get
  let m = world ^. getter . tableMap
  return $ Map.lookup k m
