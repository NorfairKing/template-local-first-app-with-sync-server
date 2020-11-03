{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.API.Server.Handler.Sync where

import qualified Data.Appendful.Persistent as Appendful
import qualified Data.Mergeful.Persistent as Mergeful
import qualified Data.Mergeless.Persistent as Mergeless
import Foo.Bar.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} SyncRequest {..} = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $ do
    syncResponseAppendfulThingSyncResponse <-
      Appendful.serverProcessSyncQuery
        [ServerAppendfulThingUser ==. uid]
        serverAppendfulMakeThing
        (makeServerAppendfulThing uid)
        syncRequestAppendfulThingSyncRequest
    syncResponseMergelessThingSyncResponse <-
      Mergeless.serverProcessSyncQuery
        [ServerMergelessThingUser ==. uid]
        serverMergelessMakeThing
        (makeServerMergelessThing uid)
        syncRequestMergelessThingSyncRequest
    syncResponseMergefulThingSyncResponse <-
      Mergeful.serverProcessSyncQuery
        ServerMergefulThingTime
        [ServerMergefulThingUser ==. uid]
        serverMergefulMakeThing
        (const $ makeServerMergefulThing uid)
        (\Thing {..} -> [ServerMergefulThingNumber =. thingNumber])
        syncRequestMergefulThingSyncRequest
    pure SyncResponse {..}
