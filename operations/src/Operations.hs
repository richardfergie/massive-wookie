{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Operations where
import Control.Monad.Operational hiding (view)
import qualified Control.Monad.Operational as Op
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens

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

insertWorld :: (Key a ~ Int) => Setter' World (Table a) -> Getter World (Table a) -> a -> State World (Entity a)
insertWorld setter getter p = do
  world <- get
  let k = world ^. getter . maxKey
      m = world ^. getter . tableMap
      newm = Map.insert k p m
      newtable = Table (k+1) newm
      newworld = world & setter .~ newtable
  put newworld
  return $ Entity k p

updateWorld :: (Key a ~ Int) => Setter' World (Table a) -> Getter World (Table a) -> Key a -> a -> State World (Entity a)
updateWorld setter getter k v = do
  world <- get
  let m = world ^. getter . tableMap
      newm = Map.insert k v m
      newworld = world & setter . tableMap .~ newm
  put newworld
  return $ Entity k v

getWorld :: (Key a ~ Int) => Getter World (Table a) -> Key a -> State World (Maybe (Entity a))
getWorld getter k = do
  world <- get
  let m = world ^. getter . tableMap
  return $ fmap (Entity k) $ Map.lookup k m

data Actions a where
  CreateProject :: FacilitatorId -> GroupId -> Actions (Entity Project)
  CreateGroupMember :: Actions (Entity GroupMember)
  CreateGroup :: OrganisationId -> [GroupMemberId] -> Actions (Entity Group)
  SubmitProject :: ProjectId -> Actions (Entity Project)
  ValidateProject :: ProjectId -> Actions (Entity Project)
  CreateCommunityPanelMember :: Actions (Entity CommunityPanelMember)
  CreatePanel :: ProjectId -> FacilitatorId -> Either FacilitatorId CommunityPanelMemberId -> Actions (Entity Panel)
  CreatePanelReport :: PanelId -> Actions (Entity Panel) --update panel to include report or have this separate?
  PanelApproveProject :: ProjectId -> Actions (Entity Project)
  GrantProject :: ProjectId -> Actions (Entity Project)

type Flow = Program Actions

createProject x y = singleton $ CreateProject x y

createGroupMember :: Flow (Entity GroupMember)
createGroupMember = singleton CreateGroupMember

createGroup :: OrganisationId -> [GroupMemberId] -> Flow (Entity Group)
createGroup x y = singleton $ CreateGroup x y

submitProject :: ProjectId -> Flow (Entity Project)
submitProject = singleton . SubmitProject

validateProject :: ProjectId -> Flow (Entity Project)
validateProject = singleton . ValidateProject

createCommunityPanelMember = singleton CreateCommunityPanelMember

createPanel x y z = singleton $ CreatePanel x y z

stateFlow :: Flow a -> State World a
stateFlow p = case Op.view p of
  Return a -> return a
  (CreateGroupMember :>>= ps) ->
    insertWorld worldGroupMembers worldGroupMembers GroupMember >>= stateFlow . ps
  (CreateGroup oid ms :>>= ps) ->
    insertWorld worldGroups worldGroups (Group oid ms) >>= stateFlow . ps
  (CreateProject fid gid :>>= ps) ->
    insertWorld worldProjects worldProjects (Project fid gid Nothing Created) >>= stateFlow . ps

example :: Flow ProjectId
example = do
  g1 <- createGroupMember
  g2 <- createGroupMember
  g3 <- createGroupMember
  Entity g _ <- createGroup 1 $ map entityKey [g1,g2,g3]
  Entity pid proj <- createProject 1 g
  return pid
