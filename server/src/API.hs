{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module API where

import Servant
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Crud
import qualified Types
import Foundation
import Auth
import Control.Monad.Reader(ask)

type CRUD a =  Get '[JSON] [Types.Entity a]
               :<|> "create" :> ReqBody '[JSON] a :> Post '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> DeleteNoContent '[JSON] NoContent
               :<|> Capture "id" Int :> ReqBody '[JSON] a :> Put '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> Get '[JSON] (Types.Entity a)

type API = "groupmember" :> CRUD Types.GroupMember
        :<|> "group" :> CRUD Types.Group
        :<|> "project" :> CRUD Types.Project
        :<|> "organisation" :> CRUD Types.Organisation
        :<|> "facilitator" :> CRUD Types.Facilitator
        :<|> "current" :> ("facilitator" :> Get '[JSON] (Types.Entity Types.Facilitator)
                      :<|> "eligiblegroups" :> Get '[JSON] [Types.Entity Types.Group]
                           )

crudder creator deleter setter getter = (\ i -> requireUser >> (runDB $ creator i))
             :<|> (\i -> requireUser >> (runDB $ deleter i) >> return NoContent)
             :<|> (\k v -> requireUser >> (runDB $ setter k v))
             :<|> (\k -> do
                     res <- runDB $ getter k
                     case res of
                       Nothing -> throwError err404
                       Just r -> return r
                  )

groupsByUserId uid = do
  mfac <- Crud.getFacilitatorByUserId uid
  case mfac of
    Nothing -> return []
    Just (Types.Entity facid (Types.Facilitator _ _ orgids)) ->
      fmap concat $ mapM Crud.getGroupsByOrganisationId orgids

crudGroup :: ServerT (CRUD Types.Group) (AppServer)
crudGroup = (requireUser >>= runDB . groupsByUserId . _userCredsId)
            :<|> (crudder Crud.createGroup
                          Crud.deleteGroup
                          Crud.setGroup
                          Crud.getGroup)

crudGroupMember :: ServerT (CRUD Types.GroupMember) (AppServer)
crudGroupMember = return [] :<|> (crudder Crud.createGroupMember
                                          Crud.deleteGroupMember
                                          Crud.setGroupMember
                                          Crud.getGroupMember)

projectsByUserId uid = do
  mfac <- Crud.getFacilitatorByUserId uid
  case mfac of
    Nothing -> return [] --maybe throw error because they are not a facilitator?
    Just (Types.Entity _ f) -> fmap concat $
                              mapM Crud.getProjectsByOrganisationId $
                              Types.facilitatorOrganisations f

crudProject :: ServerT (CRUD Types.Project) (AppServer)
crudProject = (requireUser >>= runDB . projectsByUserId . _userCredsId) :<|> (crudder Crud.createProject
                                      Crud.deleteProject
                                      Crud.setProject
                                      Crud.getProject)

crudFacilitator :: ServerT (CRUD Types.Facilitator) (AppServer)
crudFacilitator = return [] :<|> (crudder Crud.createFacilitator
                                          Crud.deleteFacilitator
                                          Crud.setFacilitator
                                          Crud.getFacilitator)

getUserOrganisations uid = do
  mfac <- Crud.getFacilitatorByUserId uid
  case mfac of
    Nothing -> return []
    Just fac -> Crud.getOrganisationsByFacilitatorId $ Types.entityKey fac

crudOrganisation :: ServerT (CRUD Types.Organisation) (AppServer)
crudOrganisation = (fmap _userCredsId requireUser >>= runDB . getUserOrganisations)
  :<|> (crudder Crud.createOrganisation
                Crud.deleteOrganisation
                Crud.setOrganisation
                Crud.getOrganisation)

getUserFacilitator :: AppServer (Types.Entity Types.Facilitator)
getUserFacilitator = do
  uid <- fmap _userCredsId requireUser
  mfac <- runDB $ Crud.getFacilitatorByUserId uid
  case mfac of
    Nothing -> throwError err404
    Just fac -> return fac

-- groups that are eligible to have a project created
-- i.e. no currently active project
getEligibleGroups :: AppServer [Types.Entity Types.Group]
getEligibleGroups = return []

apiServer :: ServerT API (AppServer)
apiServer = crudGroupMember
          :<|> crudGroup
          :<|> crudProject
          :<|> crudOrganisation
          :<|> crudFacilitator
          :<|> getUserFacilitator
          :<|> getEligibleGroups
