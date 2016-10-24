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

import Types
import qualified Crud


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
