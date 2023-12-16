{-# LANGUAGE DataKinds #-}
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

module Foo.Bar.Client.Data.DB where

import Data.Mergeful
import Data.Mergeful.Persistent ()
import Database.Persist.Sqlite
import Database.Persist.TH
import Foo.Bar.API.Server.Data
import Foo.Bar.Data

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientAppendfulThing sql=appendful_thing
    number Int
    serverId ServerAppendfulThingId Maybe
    deriving Show Eq


ClientMergelessThing sql=mergeless_thing
    number Int

    serverId ServerMergelessThingId Maybe
    deletedLocally Bool
    deriving Show Eq


ClientMergefulThing sql=mergeful_thing
    number Int

    serverId ServerMergefulThingId Maybe
    serverTime ServerTime Maybe
    changedLocally Bool
    deletedLocally Bool
    deriving Show Eq
|]

clientAppendfulMakeThing :: ClientAppendfulThing -> Thing
clientAppendfulMakeThing ClientAppendfulThing {..} = Thing {..}
  where
    thingNumber = clientAppendfulThingNumber

makeSyncedClientAppendfulThing :: ServerAppendfulThingId -> Thing -> ClientAppendfulThing
makeSyncedClientAppendfulThing sid = makeClientAppendfulThing (Just sid)

makeUnsyncedClientAppendfulThing :: Thing -> ClientAppendfulThing
makeUnsyncedClientAppendfulThing = makeClientAppendfulThing Nothing

makeClientAppendfulThing :: Maybe ServerAppendfulThingId -> Thing -> ClientAppendfulThing
makeClientAppendfulThing clientAppendfulThingServerId Thing {..} = ClientAppendfulThing {..}
  where
    clientAppendfulThingNumber = thingNumber

clientMergelessMakeThing :: ClientMergelessThing -> Thing
clientMergelessMakeThing ClientMergelessThing {..} = Thing {..}
  where
    thingNumber = clientMergelessThingNumber

makeSyncedClientMergelessThing :: ServerMergelessThingId -> Thing -> ClientMergelessThing
makeSyncedClientMergelessThing sid = makeClientMergelessThing (Just sid)

makeUnsyncedClientMergelessThing :: Thing -> ClientMergelessThing
makeUnsyncedClientMergelessThing = makeClientMergelessThing Nothing

makeClientMergelessThing :: Maybe ServerMergelessThingId -> Thing -> ClientMergelessThing
makeClientMergelessThing clientMergelessThingServerId Thing {..} = ClientMergelessThing {..}
  where
    clientMergelessThingDeletedLocally = False
    clientMergelessThingNumber = thingNumber

clientMergefulMakeThing :: ClientMergefulThing -> (Maybe ServerMergefulThingId, Maybe ServerTime, Thing)
clientMergefulMakeThing ClientMergefulThing {..} = (clientMergefulThingServerId, clientMergefulThingServerTime, Thing {..})
  where
    thingNumber = clientMergefulThingNumber

makeUnsyncedClientMergefulThing :: Thing -> ClientMergefulThing
makeUnsyncedClientMergefulThing = makeClientMergefulThing Nothing Nothing

makeSyncedClientMergefulThing :: ServerMergefulThingId -> Timed Thing -> ClientMergefulThing
makeSyncedClientMergefulThing sid (Timed h st) = makeClientMergefulThing (Just sid) (Just st) h

makeClientMergefulThing :: Maybe ServerMergefulThingId -> Maybe ServerTime -> Thing -> ClientMergefulThing
makeClientMergefulThing clientMergefulThingServerId clientMergefulThingServerTime Thing {..} = ClientMergefulThing {..}
  where
    clientMergefulThingDeletedLocally = False
    clientMergefulThingChangedLocally = False
    clientMergefulThingNumber = thingNumber
