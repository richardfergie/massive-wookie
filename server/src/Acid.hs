{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Acid where

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask, reader, runReader )
import Control.Monad.State  ( get, put, runState, state, MonadState )
import Data.Data            ( Data, Typeable )
import Data.Acid            --( AcidState, Query, Update
                            --, makeAcidic, openLocalState, liftQuery )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy, SafeCopy )
import Data.IxSet.Typed
import Control.Lens (Lens', makeLenses)
import qualified Control.Lens as Lens
import qualified Data.Serialize as S
import Data.Serialize.Text ()
import GHC.Generics
import Data.Time
import Control.Monad.Operational hiding (view)
import qualified Control.Monad.Operational as Op
import Control.Monad.Except
import Data.Maybe
import Data.Either
import Control.Monad.Trans.Either

import Types (Entity, GroupMember, GroupMemberId, Group, GroupId)
import qualified Types
import qualified Crud

instance S.Serialize Day where
  put = S.put . toModifiedJulianDay
  get = ModifiedJulianDay <$> S.get

deriving instance (S.Serialize a) => S.Serialize (Entity a)
deriving instance (S.Serialize a, SafeCopy a) => (SafeCopy (Entity a))

data Table ixs a = Table {
  _maxKey :: Int,
  _table :: IxSet ixs (Entity a)
  } deriving (Generic)

deriving instance (Indexable ixs (Entity a), Show a) => Show (Table ixs a)

instance (S.Serialize a, Indexable ixs (Entity a)) => S.Serialize (Table ixs a) where
  put (Table k t) = S.put k >> S.put (toList t)
  get = Table <$> S.get <*> fmap fromList S.get

deriving instance (S.Serialize a, SafeCopy a, Indexable ixs (Entity a)) => SafeCopy (Table ixs a)

makeLenses ''Table

--tableCreate :: (Indexable ixs (Entity a), IsIndexOf Int ixs) =>
--     a -> Update (Table ixs a) (Entity a)
tableCreate x = do
  Table k ix <- get
  put $ Table (k+1) $ updateIx k (Types.Entity k x) ix
  return $ Types.Entity k x

--tableDelete :: (Indexable ixs (Entity a), IsIndexOf Int ixs) =>
--     Int -> Update (Table ixs a) ()
tableDelete i = do
  Table k ix <- get
  put $ Table k $ deleteIx i ix
  return ()

--tableUpdate :: (Indexable ixs (Entity a), IsIndexOf Int ixs) =>
--     Int -> a -> Update (Table ixs a) (Entity a)
tableUpdate k v = do
  Table mk ix <- get
  put $ Table mk $ updateIx k (Types.Entity k v) ix
  return $ Types.Entity k v

--tableGet :: (Indexable ixs (Entity a), IsIndexOf Int ixs) =>
--     Int -> Query (Table ixs a) (Maybe (Entity a))
tableGet k = do
  Table _ ix <- ask
  return $ getOne $ getEQ k ix

emptyTable :: (Indexable ixs (Entity a)) => Table ixs a
emptyTable = Table 0 empty

type GroupMemberIxs = '[Types.GroupMemberId]

instance Indexable GroupMemberIxs (Entity GroupMember) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])

type GroupIxs = '[Types.GroupId, Types.OrganisationId]

instance Indexable GroupIxs (Entity Types.Group) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])
                   (ixFun $ \x -> [Types.organisation $ Types.entityVal x])

type OrganisationIxs = '[Types.OrganisationId]

instance Indexable OrganisationIxs (Entity Types.Organisation) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])

type UserIxs = '[Types.UserId,Types.Username]

instance Indexable UserIxs (Entity Types.User) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])
                   (ixFun $ \x -> [Types.username $ Types.entityVal x])

type FacilitatorIxs = '[Types.FacilitatorId,Types.UserId, Types.OrganisationId]

instance Indexable FacilitatorIxs (Entity Types.Facilitator) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])
                   (ixFun $ \x -> [Types.facilitatorUser $ Types.entityVal x])
                   (ixFun $ \x -> Types.facilitatorOrganisations $ Types.entityVal x)

type ProjectIxs = '[Types.ProjectId, Types.FacilitatorId]

instance Indexable ProjectIxs (Entity Types.Project) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])
                   (ixFun $ \x -> [Types.facilitator $ Types.entityVal x])

data World = World {
  _worldGroupMembers :: Table GroupMemberIxs Types.GroupMember,
  _worldGroups :: Table GroupIxs Types.Group,
  _worldOrganisations :: Table OrganisationIxs Types.Organisation,
  _worldUsers :: Table UserIxs Types.User,
  _worldFacilitators :: Table FacilitatorIxs Types.Facilitator,
  _worldProjects :: Table ProjectIxs Types.Project
             } deriving (Show)

makeLenses ''World

emptyWorld :: World
emptyWorld = World emptyTable
                   emptyTable
                   emptyTable
                   emptyTable
                   emptyTable
                   emptyTable

worldAction lens action = state . runState . (Lens.zoom lens) . action

insertWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> a -> Update World (Entity a)
insertWorld lens = worldAction lens tableCreate

deleteWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> Int -> Update World ()
deleteWorld lens = worldAction lens tableDelete

updateWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> Int -> a -> Update World (Entity a)
updateWorld lens k v = state $ runState $ Lens.zoom lens (tableUpdate k v)

getWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens.Getter World (Table ixs a) -> Int -> Query World (Maybe (Entity a))
getWorld lens k = reader $ runReader $ Lens.magnify lens (tableGet k)

--
-- User queries
--
insertUser = insertWorld worldUsers
deleteUser = deleteWorld worldUsers
updateUser = updateWorld worldUsers
getUser = getWorld worldUsers

getUserByUsername :: Types.Username -> Query World (Maybe (Entity Types.User))
getUserByUsername u = do
  Table _ users <- fmap _worldUsers ask
  return $ getOne $ getEQ u users

deriving instance S.Serialize Types.User
$(deriveSafeCopy 0 'base ''Types.User)

--
-- GroupMember queries
--
insertGroupMember = insertWorld worldGroupMembers
deleteGroupMember = deleteWorld worldGroupMembers
updateGroupMember = updateWorld worldGroupMembers
getGroupMember = getWorld worldGroupMembers
deriving instance S.Serialize GroupMember
$(deriveSafeCopy 0 'base ''GroupMember)

--
-- Organisation queries
--
insertOrganisation = insertWorld worldOrganisations
deleteOrganisation = deleteWorld worldOrganisations
updateOrganisation = updateWorld worldOrganisations
getOrganisation = getWorld worldOrganisations

getOrganisationsByFacilitatorId :: Types.FacilitatorId -> Query World [Types.Entity Types.Organisation]
getOrganisationsByFacilitatorId fid = do
  mfac <- getFacilitator fid
  case mfac of
    Nothing -> return []
    Just (Types.Entity _ fac) -> do
      orgs <- mapM getOrganisation $ Types.facilitatorOrganisations fac
      return $ catMaybes orgs

--
-- Facilitator queries
--
deleteFacilitator = deleteWorld worldFacilitators
getFacilitator = getWorld worldFacilitators

getFacilitatorByUserId :: Types.UserId -> Query World (Maybe (Types.Entity Types.Facilitator))
getFacilitatorByUserId uid = do
  Table _ facilitators <- fmap _worldFacilitators ask
  return $ getOne $ getEQ uid facilitators

checkFacilitatorKeys f = do
  ukey <- fmap isJust $ getUser $ Types.facilitatorUser f
  orgkeys <- fmap (all isJust) $ mapM getOrganisation $ Types.facilitatorOrganisations f
  return $ ukey && orgkeys

insertFacilitator f = do
  fkey <- liftQuery $ checkFacilitatorKeys f
  if fkey
    then Right <$> insertWorld worldFacilitators f
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"

updateFacilitator k v = do
  fkey <- liftQuery $ checkFacilitatorKeys v
  if fkey
    then Right <$> updateWorld worldFacilitators k v
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"


deriving instance S.Serialize Types.Facilitator
$(deriveSafeCopy 0 'base ''Types.Facilitator)

--
-- Group Queries
--
deriving instance S.Serialize Group
$(deriveSafeCopy 0 'base ''Group)

checkGroupMemberIds :: [GroupMemberId] -> Query World Bool
checkGroupMemberIds xs = do
  mids <- mapM getGroupMember xs
  return $ all isJust mids

deleteGroup = deleteWorld worldGroups
getGroup = getWorld worldGroups

insertGroup g = do
  fkey <- liftQuery $ checkGroupMemberIds $ Types.members g
  if fkey
    then fmap Right $ insertWorld worldGroups g
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"

updateGroup k v = do
  fkey <- liftQuery $ checkGroupMemberIds $ Types.members v
  if fkey
    then fmap Right $ updateWorld worldGroups k v
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"

getGroupsByOrganisationId :: Types.OrganisationId
  -> Query World [Types.Entity Types.Group]
getGroupsByOrganisationId oid = do
  Table _ groups <- fmap _worldGroups ask
  return $ toList $ getEQ oid groups


--
-- Project queries
--
deriving instance S.Serialize Types.ProjectStatus
$(deriveSafeCopy 0 'base ''Types.ProjectStatus)

deriving instance S.Serialize Types.Project
$(deriveSafeCopy 0 'base ''Types.Project)

deleteProject = deleteWorld worldProjects
getProject = getWorld worldProjects

getProjectsByFacilitatorId :: Types.FacilitatorId -> Query World [Types.Entity Types.Project]
getProjectsByFacilitatorId fid = do
  Table _ projects <- fmap _worldProjects ask
  return $ toList $ getEQ (fid::Types.FacilitatorId) projects

checkProjectKeys p = do
  fkey <- fmap isJust $ getFacilitator $ Types.facilitator p
  gkey <- fmap isJust $ getGroup $ Types.group p
  pkey <- case Types.panel p of
    Nothing -> return True
    Just pid -> fmap isJust $ error "Panel stuff is not defined"
  return $ fkey && gkey && pkey

insertProject p = do
  fkey <- liftQuery $ checkProjectKeys p
  if fkey
    then Right <$> insertWorld worldProjects p
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"

updateProject k v = do
  fkey <- liftQuery $ checkProjectKeys v
  if fkey
    then Right <$> updateWorld worldProjects k v
    else return $ Left $ Crud.ForeignKeyMissing "Missing foreign key"

gWorld :: Query World World
gWorld = ask

deriving instance S.Serialize Types.Organisation
$(deriveSafeCopy 0 'base ''Types.Organisation)

$(deriveSafeCopy 0 'base ''World)

$(deriveSafeCopy 0 'base ''Crud.CRUDError)


$(makeAcidic ''World ['insertGroup,
                      'deleteGroup,
                      'updateGroup,
                      'getGroup,
                      'insertGroupMember,
                      'deleteGroupMember,
                      'updateGroupMember,
                      'getGroupMember,
                      'getGroupsByOrganisationId,
                      'insertFacilitator,
                      'deleteFacilitator,
                      'updateFacilitator,
                      'getFacilitator,
                      'getFacilitatorByUserId,
                      'insertProject,
                      'deleteProject,
                      'updateProject,
                      'getProject,
                      'getProjectsByFacilitatorId,
                      'insertOrganisation,
                      'deleteOrganisation,
                      'updateOrganisation,
                      'getOrganisation,
                      'getOrganisationsByFacilitatorId,
                      'insertUser,
                      'deleteUser,
                      'updateUser,
                      'getUser,
                      'getUserByUsername,
                      'gWorld
                     ])

printWorld acid = query' acid GWorld >>= liftIO . print

acidStateCrud :: (Monad m, MonadIO m) => AcidState World -> ProgramT Crud.Crud m a -> m (Either Crud.CRUDError a)
acidStateCrud acid p = do
 y <- Op.viewT p
 case y of
  Return x -> return $ Right x

  Crud.GetGroup x :>>= ps -> query' acid (GetGroup x) >>= acidStateCrud acid . ps
  Crud.DeleteGroup x :>>= ps -> update' acid (DeleteGroup x) >>= acidStateCrud acid . ps
  Crud.SetGroup k v :>>= ps -> update' acid (UpdateGroup k v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.CreateGroup v :>>= ps -> update' acid (InsertGroup v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.GetGroupsByOrganisationId oid :>>= ps -> query' acid (GetGroupsByOrganisationId oid) >>= acidStateCrud acid . ps

  Crud.GetGroupMember x :>>= ps -> query' acid (GetGroupMember x) >>= acidStateCrud acid . ps
  Crud.DeleteGroupMember x :>>= ps -> update' acid (DeleteGroupMember x) >>= acidStateCrud acid . ps
  Crud.SetGroupMember k v :>>= ps -> update' acid (UpdateGroupMember k v) >>= acidStateCrud acid . ps
  Crud.CreateGroupMember v :>>= ps -> update' acid (InsertGroupMember v) >>= acidStateCrud acid . ps

  Crud.GetOrganisation x :>>= ps -> query' acid (GetOrganisation x) >>= acidStateCrud acid . ps
  Crud.DeleteOrganisation x :>>= ps -> update' acid (DeleteOrganisation x) >>= acidStateCrud acid . ps
  Crud.SetOrganisation k v :>>= ps -> update' acid (UpdateOrganisation k v) >>= acidStateCrud acid . ps
  Crud.CreateOrganisation v :>>= ps -> update' acid (InsertOrganisation v) >>= acidStateCrud acid . ps
  Crud.GetOrganisationsByFacilitatorId f :>>= ps -> query' acid (GetOrganisationsByFacilitatorId f) >>= acidStateCrud acid . ps

  Crud.GetFacilitator x :>>= ps -> query' acid (GetFacilitator x) >>= acidStateCrud acid . ps
  Crud.DeleteFacilitator x :>>= ps -> update' acid (DeleteFacilitator x) >>= acidStateCrud acid . ps
  Crud.SetFacilitator k v :>>= ps -> update' acid (UpdateFacilitator k v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.CreateFacilitator v :>>= ps -> update' acid (InsertFacilitator v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.GetFacilitatorByUserId uid :>>= ps -> query' acid (GetFacilitatorByUserId uid) >>= acidStateCrud acid . ps

  Crud.GetProject x :>>= ps -> query' acid (GetProject x) >>= acidStateCrud acid . ps
  Crud.DeleteProject x :>>= ps -> update' acid (DeleteProject x) >>= acidStateCrud acid . ps
  Crud.SetProject k v :>>= ps -> update' acid (UpdateProject k v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.CreateProject v :>>= ps -> update' acid (InsertProject v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.GetProjectsByFacilitatorId fid :>>= ps -> query' acid (GetProjectsByFacilitatorId fid) >>= acidStateCrud acid . ps

  Crud.GetUser x :>>= ps -> query' acid (GetUser x) >>= acidStateCrud acid . ps
  Crud.DeleteUser x :>>= ps -> update' acid (DeleteUser x) >>= acidStateCrud acid . ps
  Crud.SetUser k v :>>= ps -> update' acid (UpdateUser k v) >>= acidStateCrud acid . ps
  Crud.CreateUser v :>>= ps -> update' acid (InsertUser v) >>= acidStateCrud acid . ps
  Crud.GetUserByUsername u :>>= ps -> query' acid (GetUserByUsername u) >>= acidStateCrud acid . ps
