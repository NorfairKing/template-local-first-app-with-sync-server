{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.API.Server.Handler.Sync where

import Data.Appendful.Persistent
import Foo.Bar.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} SyncRequest {..} = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $ do
    syncResponseThingSyncResponse <- serverProcessSyncQuery [ServerThingUser ==. uid] serverMakeThing (makeServerThing uid) syncRequestThingSyncRequest
    pure SyncResponse {..}
