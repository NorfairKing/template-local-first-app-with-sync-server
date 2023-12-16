{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Server.Data.DB where

import Data.Mergeful
import Data.Mergeful.Persistent ()
import Data.Password.Bcrypt
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
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerAppendfulThing sql=appendful_thing
  user UserId
  number Int

  deriving Show Eq Ord Generic

ServerMergelessThing sql=mergeless_thing
  user UserId
  number Int

  deriving Show Eq Ord Generic

ServerMergefulThing sql=mergeful_thing
  user UserId
  number Int
  time ServerTime

  deriving Show Eq Ord Generic

|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity Password where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

instance Validity ServerAppendfulThing

instance Validity ServerMergelessThing

instance Validity ServerMergefulThing

serverAppendfulMakeThing :: ServerAppendfulThing -> Thing
serverAppendfulMakeThing ServerAppendfulThing {..} = Thing {..}
  where
    thingNumber = serverAppendfulThingNumber

makeServerAppendfulThing :: UserId -> Thing -> ServerAppendfulThing
makeServerAppendfulThing serverAppendfulThingUser Thing {..} = ServerAppendfulThing {..}
  where
    serverAppendfulThingNumber = thingNumber

serverMergelessMakeThing :: ServerMergelessThing -> Thing
serverMergelessMakeThing ServerMergelessThing {..} = Thing {..}
  where
    thingNumber = serverMergelessThingNumber

makeServerMergelessThing :: UserId -> Thing -> ServerMergelessThing
makeServerMergelessThing serverMergelessThingUser Thing {..} = ServerMergelessThing {..}
  where
    serverMergelessThingNumber = thingNumber

serverMergefulMakeThing :: ServerMergefulThing -> Timed Thing
serverMergefulMakeThing ServerMergefulThing {..} = Timed Thing {..} serverMergefulThingTime
  where
    thingNumber = serverMergefulThingNumber

makeServerMergefulThing :: UserId -> Thing -> ServerMergefulThing
makeServerMergefulThing serverMergefulThingUser Thing {..} = ServerMergefulThing {..}
  where
    serverMergefulThingTime = initialServerTime
    serverMergefulThingNumber = thingNumber
