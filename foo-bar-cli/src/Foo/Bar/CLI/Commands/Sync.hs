{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.CLI.Commands.Sync where

import qualified Data.Appendful.Persistent as Appendful
import Data.Maybe
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Persistent as Mergeful
import qualified Data.Mergeless.Persistent as Mergeless
import Foo.Bar.CLI.Commands.Import

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ do
    syncRequestAppendfulThingSyncRequest <-
      Appendful.clientMakeSyncRequestQuery
        clientAppendfulMakeThing
        ClientAppendfulThingServerId
    syncRequestMergelessThingSyncRequest <-
      Mergeless.clientMakeSyncRequestQuery
        clientMergelessMakeThing
        ClientMergelessThingServerId
        ClientMergelessThingDeletedLocally
    syncRequestMergefulThingSyncRequest <-
      Mergeful.clientMakeSyncRequestQuery
        ClientMergefulThingServerId
        ClientMergefulThingServerTime
        ClientMergefulThingChangedLocally
        ClientMergefulThingDeletedLocally
        ((\(_, _, h) -> h) . clientMergefulMakeThing)
        ((\(msid, mst, h) -> (fromJust msid, Mergeful.Timed h (fromJust mst))) . clientMergefulMakeThing)
        ((\(msid, mst, _) -> (fromJust msid, fromJust mst)) . clientMergefulMakeThing)
    pure SyncRequest {..}
  SyncResponse {..} <- runClientOrDie cenv $ postSync fooBarClient token req
  runDB $ do
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientAppendfulThing
      ClientAppendfulThingServerId
      syncResponseAppendfulThingSyncResponse
    Mergeless.clientMergeSyncResponseQuery
      makeSyncedClientMergelessThing
      ClientMergelessThingServerId
      ClientMergelessThingDeletedLocally
      syncResponseMergelessThingSyncResponse
    Mergeful.clientMergeSyncResponseQuery
      ClientMergefulThingServerId
      ClientMergefulThingServerTime
      ClientMergefulThingChangedLocally
      ClientMergefulThingDeletedLocally
      makeSyncedClientMergefulThing
      ((\(msid, mst, h) -> (fromJust msid, Mergeful.Timed h (fromJust mst))) . clientMergefulMakeThing)
      ( \Thing {..} ->
          [ ClientMergefulThingNumber =. thingNumber
          ]
      )
      Mergeful.mergeFromServerStrategy
      syncResponseMergefulThingSyncResponse
