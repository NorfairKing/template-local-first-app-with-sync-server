{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foobar.API.Server.Data.DB where

import Data.Password
import Data.Password.Instances ()
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import Foobar.API.Server.Data.Username
import Foobar.Data
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password PassHash

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerThing sql=thing
  user UserId
  number Int

  deriving Show Eq Ord Generic

|]

instance Validity Salt where
  validate = trivialValidation

instance Validity Pass where
  validate = trivialValidation

instance Validity PassHash where
  validate = trivialValidation

instance Validity User

instance Validity ServerThing

serverMakeThing :: ServerThing -> Thing
serverMakeThing ServerThing {..} = Thing {..}
  where
    thingNumber = serverThingNumber

makeServerThing :: UserId -> Thing -> ServerThing
makeServerThing serverThingUser Thing {..} = ServerThing {..}
  where
    serverThingNumber = thingNumber
