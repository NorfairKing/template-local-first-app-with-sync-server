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

ClientThing sql=thing
    number Int
    serverId ServerThingId Maybe
    deriving Show Eq
|]

clientMakeThing :: ClientThing -> Thing
clientMakeThing ClientThing {..} = Thing {..}
  where
    thingNumber = clientThingNumber

makeSyncedClientThing :: ServerThingId -> Thing -> ClientThing
makeSyncedClientThing sid = makeClientThing (Just sid)

makeUnsyncedClientThing :: Thing -> ClientThing
makeUnsyncedClientThing = makeClientThing Nothing

makeClientThing :: Maybe ServerThingId -> Thing -> ClientThing
makeClientThing clientThingServerId Thing {..} = ClientThing {..}
  where
    clientThingNumber = thingNumber
