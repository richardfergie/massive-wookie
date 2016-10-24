{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Crud where

import Control.Monad.Operational hiding (view)
import qualified Control.Monad.Operational as Op
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens

import Types

data Crud a where
  CreateFacilitator :: Facilitator -> Crud (Entity Facilitator)
  SetFacilitator :: FacilitatorId -> Facilitator -> Crud (Entity Facilitator)
  DeleteFacilitator :: FacilitatorId -> Crud ()
  GetFacilitator :: FacilitatorId -> Crud (Maybe Facilitator)
  CreateGroup :: Group -> Crud (Entity Group)
  SetGroup :: GroupId -> Group -> Crud (Entity Group)
  DeleteGroup :: GroupId -> Crud ()
  GetGroup :: GroupId -> Crud (Maybe Group)
  CreateProject :: Project -> Crud (Entity Project)
  SetProject :: ProjectId -> Project -> Crud (Entity Project)
  DeleteProject :: ProjectId -> Crud ()
  GetProject :: ProjectId -> Crud (Maybe Project)
  CreateGroupMember :: GroupMember -> Crud (Entity GroupMember)
  SetGroupMember :: GroupMemberId -> GroupMember -> Crud (Entity GroupMember)
  DeleteGroupMember :: GroupMemberId -> Crud ()
  GetGroupMember :: GroupMemberId -> Crud (Maybe GroupMember)

createFacilitator = singleton . CreateFacilitator
setFacilitator = singleton . SetFacilitator
deleteFacilitator = singleton . DeleteFacilitator
getFacilitator = singleton . GetFacilitator

createGroup = singleton . CreateGroup
setGroup = singleton . SetGroup
deleteGroup = singleton . DeleteGroup
getGroup = singleton . GetGroup

createProject = singleton . CreateProject
setProject = singleton . SetProject
deleteProject = singleton . DeleteProject
getProject = singleton . GetProject

createGroupMember = singleton . CreateGroupMember
setGroupMember = singleton . SetGroupMember
deleteGroupMember = singleton . DeleteGroupMember
getGroupMember = singleton . GetGroupMember

type CRUD = Program Crud

stateCrud :: (MonadState World m) => CRUD a -> m a
stateCrud p = case Op.view p of
  Return x -> return x
  (GetProject x :>>= ps) -> getWorld worldProjects x >>= stateCrud . ps
  (GetGroup x :>>= ps) -> getWorld worldGroups x >>= stateCrud . ps
  (GetGroupMember x :>>= ps) -> getWorld worldGroupMembers x >>= stateCrud . ps
  (CreateProject x :>>= ps) -> do
    --also need facilitator check here
    mgroup <- stateCrud $ getGroup $ group x
    case mgroup of
      Just _ -> insertWorld worldProjects x >>= stateCrud . ps
      Nothing -> error "GroupId does not exist"
  (CreateGroupMember x :>>= ps) -> insertWorld worldGroupMembers x >>= stateCrud . ps
  (CreateGroup x :>>= ps) -> do
    --also need org check here
    mgmids <- stateCrud $ traverse getGroupMember $ members x
    case all isJust mgmids of
      True -> insertWorld worldGroups x >>= stateCrud . ps
      False -> error "At least one group member doesn't exist"

example :: CRUD ProjectId
example = do
  g1 <- createGroupMember GroupMember
  g2 <- createGroupMember GroupMember
  g3 <- createGroupMember GroupMember
  Entity g _ <- createGroup $ Group 1 $ map entityKey [g1,g2,g3]
  Entity pid _ <- createProject $ Project 1 g Nothing Created
  return pid
