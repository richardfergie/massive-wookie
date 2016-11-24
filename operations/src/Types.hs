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

data ProjectStatus = Created | Submitted | Validated | Granted deriving (Show, Generic)
data Project = Project { facilitator :: FacilitatorId,
                         group :: GroupId,
                         panel :: Maybe PanelId,
                         status :: ProjectStatus
                       } deriving (Show, Generic)

data GroupMember = GroupMember {
  firstname :: Text,
  lastname :: Text,
  dob :: Day
                               } deriving (Show, Generic, ToJSON, FromJSON)

data Group = Group { organisation :: OrganisationId,
                     members :: [GroupMemberId],
                     name :: Text
                   } deriving (Show, Generic, ToJSON, FromJSON)

data Panel = Panel { panelGroup :: GroupId,
                     panelFacilitator :: FacilitatorId,
                     otherPanelMember :: Either FacilitatorId CommunityPanelMemberId
                   } deriving (Show,Generic, ToJSON, FromJSON)

data CommunityPanelMember = CommunityPanelMember deriving (Show,Generic)

data Facilitator = Facilitator deriving (Show, Generic)

data Organisation = Organisation deriving (Show, Generic)

{-
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
-}
