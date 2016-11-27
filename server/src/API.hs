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

type CRUD a = "create" :> ReqBody '[JSON] a :> Post '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> DeleteNoContent '[JSON] NoContent
               :<|> Capture "id" Int :> ReqBody '[JSON] a :> Put '[JSON] (Types.Entity a)
               :<|> Capture "id" Int :> Get '[JSON] (Types.Entity a)

type API = "groupmember" :> CRUD Types.GroupMember
        :<|> "group" :> CRUD Types.Group
        :<|> "project" :> CRUD Types.Project
        :<|> "organisation" :> CRUD Types.Organisation
        :<|> "facilitator" :> CRUD Types.Facilitator

crudder creator deleter setter getter = (\ i -> requireUser >> (runDB $ creator i))
             :<|> (\i -> requireUser >> (runDB $ deleter i) >> return NoContent)
             :<|> (\k v -> requireUser >> (runDB $ setter k v))
             :<|> (\k -> do
                     res <- runDB $ getter k
                     case res of
                       Nothing -> throwError err404
                       Just r -> return r
                  )

crudGroup :: ServerT (CRUD Types.Group) (AppServer)
crudGroup = crudder Crud.createGroup
                    Crud.deleteGroup
                    Crud.setGroup
                    Crud.getGroup

crudGroupMember :: ServerT (CRUD Types.GroupMember) (AppServer)
crudGroupMember = crudder Crud.createGroupMember
                          Crud.deleteGroupMember
                          Crud.setGroupMember
                          Crud.getGroupMember

crudProject :: ServerT (CRUD Types.Project) (AppServer)
crudProject = crudder Crud.createProject
                      Crud.deleteProject
                      Crud.setProject
                      Crud.getProject

crudFacilitator :: ServerT (CRUD Types.Facilitator) (AppServer)
crudFacilitator = crudder Crud.createFacilitator
                          Crud.deleteFacilitator
                          Crud.setFacilitator
                          Crud.getFacilitator

crudOrganisation :: ServerT (CRUD Types.Organisation) (AppServer)
crudOrganisation = crudder Crud.createOrganisation
                           Crud.deleteOrganisation
                           Crud.setOrganisation
                           Crud.getOrganisation

apiServer :: ServerT API (AppServer)
apiServer = crudGroupMember
          :<|> crudGroup
          :<|> crudProject
          :<|> crudOrganisation
          :<|> crudFacilitator
