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

import Types
import TH

$(genGADT "Crud" [''Facilitator, ''Group, ''Project, ''GroupMember, ''Organisation])

type CRUD = Program Crud

data CRUDError = ForeignKeyMissing String
                   deriving (Show)

stateCrudPrint :: (Monad m, MonadError CRUDError m, MonadIO m) => CRUD a -> StateT World m a
stateCrudPrint p = case Op.view p of
  Return x -> return x
  (GetProject x :>>= ps) -> (liftIO $ putStrLn "Get Project") >> getWorld worldProjects x >>= stateCrudPrint . ps
  (GetGroup x :>>= ps) -> (liftIO $ putStrLn "Get Group") >> getWorld worldGroups x >>= stateCrudPrint . ps
  (GetGroupMember x :>>= ps) -> (liftIO $ putStrLn "Get Groupmember") >> getWorld worldGroupMembers x >>= stateCrudPrint . ps
  (GetFacilitator x :>>= ps) -> (liftIO $ putStrLn "Get Facilitator") >> getWorld worldFacilitators x >>= stateCrudPrint . ps
  (GetOrganisation x :>>= ps) -> (liftIO $ putStrLn "Get Organisation") >> getWorld worldOrganisations x >>= stateCrudPrint . ps
  (CreateProject x :>>= ps) -> do
    liftIO $ putStrLn "Create Project"
    (mgroup,mfacilitator) <- stateCrudPrint $ (,) <$> (getGroup $ group x) <*> (getFacilitator $ facilitator x)
    case (mgroup, mfacilitator) of
      (Just _,Just _) -> insertWorld worldProjects x >>= stateCrudPrint . ps
      (Nothing,_) -> throwError $ ForeignKeyMissing $ "GroupMember " ++ show (group x)++" does not exist"
      (_, Nothing) -> throwError $ ForeignKeyMissing $ "Facilitator " ++ show (facilitator x)++" does not exist"
  (CreateGroupMember x :>>= ps) -> (liftIO $ putStrLn "Create GroupMember") >> insertWorld worldGroupMembers x >>= stateCrudPrint . ps
  (CreateGroup x :>>= ps) -> do
    liftIO $ putStrLn "Create Group"
    (morg,mgmids) <- stateCrudPrint $ (,) <$> (getOrganisation $ organisation x) <*> (traverse getGroupMember $ members x)
    case (morg,all isJust mgmids) of
      (Just _,True) -> insertWorld worldGroups x >>= stateCrudPrint . ps
      (_,False) -> throwError $ ForeignKeyMissing "At least one group member doesn't exist"
      (Nothing, _) -> throwError $ ForeignKeyMissing $ "Organisation " ++ show (organisation x) ++ " does not exist"
  (CreateOrganisation x :>>= ps) -> (liftIO $ putStrLn "Create Organisation") >> insertWorld worldOrganisations x >>= stateCrudPrint . ps
  (CreateFacilitator x :>>= ps) -> (liftIO $ putStrLn "Create Facilitator") >> insertWorld worldFacilitators x >>= stateCrudPrint . ps


example :: CRUD ProjectId
example = do
  oid <- entityKey <$> createOrganisation Organisation
  fid <- entityKey <$> createFacilitator Facilitator
  g1 <- createGroupMember GroupMember
  g2 <- createGroupMember GroupMember
  g3 <- createGroupMember GroupMember
  Entity g _ <- createGroup $ Group oid $ map entityKey [g1,g2,g3]
  Entity pid _ <- createProject $ Project fid g Nothing Created
  return pid
