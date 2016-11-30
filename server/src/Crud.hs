{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Crud where

import Control.Monad.Operational hiding (view)
import qualified Control.Monad.Operational as Op
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Servant
import Control.Monad.Trans.Either

import Types
import TH

-- $(genGADT "Crud" [''Facilitator, ''Group, ''Project, ''GroupMember, ''Organisation, ''User])

data Crud a where
  CreateFacilitator :: Facilitator -> Crud (Entity Facilitator)
  SetFacilitator :: FacilitatorId
      -> Facilitator -> Crud (Entity Facilitator)
  DeleteFacilitator :: FacilitatorId -> Crud ()
  GetFacilitator :: FacilitatorId
                          -> Crud (Maybe (Entity Facilitator))
  CreateGroup :: Group -> Crud (Entity Group)
  SetGroup :: GroupId -> Group -> Crud (Entity Group)
  DeleteGroup :: GroupId -> Crud ()
  GetGroup :: GroupId -> Crud (Maybe (Entity Group))
  CreateProject :: Project -> Crud (Entity Project)
  SetProject :: ProjectId -> Project -> Crud (Entity Project)
  DeleteProject :: ProjectId -> Crud ()
  GetProject :: ProjectId -> Crud (Maybe (Entity Project))
  CreateGroupMember :: GroupMember -> Crud (Entity GroupMember)
  SetGroupMember :: GroupMemberId
                          -> GroupMember -> Crud (Entity GroupMember)
  DeleteGroupMember :: GroupMemberId -> Crud ()
  GetGroupMember :: GroupMemberId
                          -> Crud (Maybe (Entity GroupMember))
  CreateOrganisation :: Organisation -> Crud (Entity Organisation)
  SetOrganisation :: OrganisationId
                           -> Organisation -> Crud (Entity Organisation)
  DeleteOrganisation :: OrganisationId -> Crud ()
  GetOrganisation :: OrganisationId
                           -> Crud (Maybe (Entity Organisation))
  CreateUser :: User -> Crud (Entity User)
  SetUser :: UserId -> User -> Crud (Entity User)
  DeleteUser :: UserId -> Crud ()
  GetUser :: UserId -> Crud (Maybe (Entity User))
  GetUserByUsername :: Username -> Crud (Maybe (Entity User))

createFacilitator = (.) singleton CreateFacilitator
setFacilitator x y = ($) singleton (SetFacilitator x y)
deleteFacilitator = (.) singleton DeleteFacilitator
getFacilitator = (.) singleton GetFacilitator
createGroup = (.) singleton CreateGroup
setGroup x y = ($) singleton (SetGroup x y)
deleteGroup = (.) singleton DeleteGroup
getGroup = (.) singleton GetGroup
createProject = (.) singleton CreateProject
setProject x y = ($) singleton (SetProject x y)
deleteProject = (.) singleton DeleteProject
getProject = (.) singleton GetProject
createGroupMember = (.) singleton CreateGroupMember
setGroupMember x y = ($) singleton (SetGroupMember x y)
deleteGroupMember = (.) singleton DeleteGroupMember
getGroupMember = (.) singleton GetGroupMember
createOrganisation = (.) singleton CreateOrganisation
setOrganisation x y = ($) singleton (SetOrganisation x y)
deleteOrganisation = (.) singleton DeleteOrganisation
getOrganisation = (.) singleton GetOrganisation
createUser = (.) singleton CreateUser
setUser x y = ($) singleton (SetUser x y)
deleteUser = (.) singleton DeleteUser
getUser = (.) singleton GetUser
getUserByUsername = singleton . GetUserByUsername

type CRUD = Program Crud
type CRUDT = ProgramT Crud

type CrudErrorT m a = EitherT Crud.CRUDError (ProgramT Crud.Crud m) a

data CRUDError = ForeignKeyMissing String
               | NonUnique String
                   deriving (Show)
