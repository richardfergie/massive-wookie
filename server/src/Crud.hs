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

$(genGADT "Crud" [''Facilitator, ''Group, ''Project, ''GroupMember, ''Organisation, ''User])

type CRUD = Program Crud
type CRUDT = ProgramT Crud

type CrudErrorT m a = EitherT Crud.CRUDError (ProgramT Crud.Crud m) a

data CRUDError = ForeignKeyMissing String
               | NotFound
               | NonUnique String
                   deriving (Show)
