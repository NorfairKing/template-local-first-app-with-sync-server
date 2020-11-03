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

module Foo.Bar.API.Server.Data.DB where

import Data.Password
import Data.Password.Instances ()
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import Foo.Bar.API.Server.Data.Username
import Foo.Bar.Data
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password PassHash

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerAppendThing sql=append_thing
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

instance Validity ServerAppendThing

serverMakeThing :: ServerAppendThing -> Thing
serverMakeThing ServerAppendThing {..} = Thing {..}
  where
    thingNumber = serverAppendThingNumber

makeServerAppendThing :: UserId -> Thing -> ServerAppendThing
makeServerAppendThing serverAppendThingUser Thing {..} = ServerAppendThing {..}
  where
    serverAppendThingNumber = thingNumber
