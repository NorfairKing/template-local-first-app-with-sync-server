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

module Foo.Bar.Client.Data.DB where

import Database.Persist.Sqlite
import Database.Persist.TH
import Foo.Bar.API.Server.Data
import Foo.Bar.Data

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientAppendThing sql=append_thing
    number Int
    serverId ServerAppendThingId Maybe
    deriving Show Eq
|]

clientMakeThing :: ClientAppendThing -> Thing
clientMakeThing ClientAppendThing {..} = Thing {..}
  where
    thingNumber = clientAppendThingNumber

makeSyncedClientAppendThing :: ServerAppendThingId -> Thing -> ClientAppendThing
makeSyncedClientAppendThing sid = makeClientAppendThing (Just sid)

makeUnsyncedClientAppendThing :: Thing -> ClientAppendThing
makeUnsyncedClientAppendThing = makeClientAppendThing Nothing

makeClientAppendThing :: Maybe ServerAppendThingId -> Thing -> ClientAppendThing
makeClientAppendThing clientAppendThingServerId Thing {..} = ClientAppendThing {..}
  where
    clientAppendThingNumber = thingNumber
