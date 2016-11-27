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

type GroupIxs = '[Types.GroupId]

instance Indexable GroupIxs (Entity Types.Group) where
  indices = ixList (ixFun $ \x -> [Types.entityKey x])

data World = World {
  _worldGroupMembers :: Table GroupMemberIxs Types.GroupMember,
  _worldGroups :: Table GroupIxs Types.Group
             } deriving (Show)

makeLenses ''World

emptyWorld = World emptyTable emptyTable

worldAction lens action = state . runState . (Lens.zoom lens) . action

insertWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> a -> Update World (Entity a)
insertWorld lens = worldAction lens tableCreate

deleteWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> Int -> Update World ()
deleteWorld lens = worldAction lens tableDelete

updateWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens' World (Table ixs a) -> Int -> a -> Update World (Entity a)
updateWorld lens k v = state $ runState $ Lens.zoom lens (tableUpdate k v)

getWorld :: (IsIndexOf Int ixs, Indexable ixs (Entity a)) => Lens.Getter World (Table ixs a) -> Int -> Query World (Maybe (Entity a))
getWorld lens k = reader $ runReader $ Lens.magnify lens (tableGet k)


deleteGroup = deleteWorld worldGroups
getGroup = getWorld worldGroups

insertGroupMember = insertWorld worldGroupMembers
deleteGroupMember = deleteWorld worldGroupMembers
updateGroupMember = updateWorld worldGroupMembers
getGroupMember = getWorld worldGroupMembers

deriving instance S.Serialize Group
$(deriveSafeCopy 0 'base ''Group)

deriving instance S.Serialize GroupMember
$(deriveSafeCopy 0 'base ''GroupMember)
$(deriveSafeCopy 0 'base ''World)

$(deriveSafeCopy 0 'base ''Crud.CRUDError)

checkGroupMemberIds :: [GroupMemberId] -> Query World Bool
checkGroupMemberIds xs = do
  mids <- mapM getGroupMember xs
  return $ all isJust mids

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

gWorld :: Query World World
gWorld = ask

$(makeAcidic ''World ['insertGroup,
                      'deleteGroup,
                      'updateGroup,
                      'getGroup,
                      'insertGroupMember,
                      'deleteGroupMember,
                      'updateGroupMember,
                      'getGroupMember,
                      'gWorld
                     ])

printWorld acid = query' acid GWorld >>= liftIO . print

acidStateCrud :: (Monad m, MonadIO m) => AcidState World -> ProgramT Crud.Crud m a -> m (Either Crud.CRUDError a)
acidStateCrud acid p = do
 y <- Op.viewT p
 case y of
  Return x -> return $ Right x
  Crud.GetGroup x :>>= ps -> query' acid (GetGroup x) >>= maybe (return $ Left Crud.NotFound) (acidStateCrud acid . ps . Just)
  Crud.DeleteGroup x :>>= ps -> update' acid (DeleteGroup x) >>= acidStateCrud acid . ps
  Crud.SetGroup k v :>>= ps -> (liftIO $ putStrLn "Insert Group") >> update' acid (UpdateGroup k v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.CreateGroup v :>>= ps -> (liftIO $ putStrLn "Create Group") >> update' acid (InsertGroup v) >>= \res -> either (return . Left) (acidStateCrud acid . ps) res
  Crud.GetGroupMember x :>>= ps -> (liftIO $ putStrLn "Get GroupMember") >> query' acid (GetGroupMember x) >>= maybe (return $ Left Crud.NotFound) (acidStateCrud acid . ps . Just)
  Crud.DeleteGroupMember x :>>= ps -> (liftIO $ putStrLn "Delete GroupMember") >> update' acid (DeleteGroupMember x) >>= acidStateCrud acid . ps
  Crud.SetGroupMember k v :>>= ps -> (liftIO $ putStrLn "Insert GroupMember") >> update' acid (UpdateGroupMember k v) >>= acidStateCrud acid . ps
  Crud.CreateGroupMember v :>>= ps -> (liftIO $ putStrLn "Create GroupMember") >> update' acid (InsertGroupMember v) >>= acidStateCrud acid . ps
