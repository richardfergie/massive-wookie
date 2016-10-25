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
import Data.Time

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

data Facilitator = Facilitator deriving Show

data Organisation = Organisation deriving Show

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

instance EntityKey Organisation where
  type Key Organisation = OrganisationId

data Table a = Table { _maxKey :: Int,
                       _tableMap :: Map Int a
                     } deriving (Show)
makeLenses ''Table

emptyTable :: Table a
emptyTable = Table 0 Map.empty

tableInsert :: (Key a ~ Int, Monad m) => a -> StateT (Table a) m (Entity a)
tableInsert v = do
  Table k m <- get
  put $ Table (k+1) $ Map.insert k v m
  return $ Entity k v

tableUpdate :: (Key a ~ Int, Monad m) => Key a -> a -> StateT (Table a) m (Entity a)
tableUpdate k v = do
  Table x m <- get
  put $ Table x $ Map.insert k v m
  return $ Entity k v

tableLookup :: (Key a ~ Int, Monad m) => Key a -> StateT (Table a) m (Maybe a)
tableLookup k = do
  Table _ m <- get
  case Map.lookup k m of
    Nothing -> return Nothing
    Just v -> return $ Just v


data World = World { _worldProjects :: Table Project,
                     _worldGroups :: Table Group,
                     _worldPanels :: Table Panel,
                     _worldGroupMembers :: Table GroupMember,
                     _worldFacilitators :: Table Facilitator,
                     _worldCommunityPanelMembers :: Table CommunityPanelMember,
                     _worldOrganisations :: Table Organisation
                   } deriving (Show)
makeLenses ''World

emptyWorld :: World
emptyWorld = World emptyTable
                   emptyTable
                   emptyTable
                   emptyTable
                   emptyTable
                   emptyTable
                   emptyTable

zoomWorld :: (Monad m) => Lens' World a -> StateT a m b -> StateT World m b
zoomWorld = zoom

insertWorld :: (Monad m, Key a ~ Int) => Lens' World (Table a) -> a -> StateT World m (Entity a)
insertWorld l p = zoomWorld l (tableInsert p)

updateWorld :: (Key a ~ Int, Monad m) => Lens' World (Table a) -> Key a -> a -> StateT World m (Entity a)
updateWorld l k v = zoomWorld l (tableUpdate k v)

getWorld :: (Key a ~ Int, Monad m) => Lens' World (Table a) -> Key a -> StateT World m (Maybe a)
getWorld getter k = zoomWorld getter (tableLookup k)
