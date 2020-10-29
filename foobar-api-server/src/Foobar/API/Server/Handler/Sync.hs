{-# LANGUAGE RecordWildCards #-}

module Foobar.API.Server.Handler.Sync where

import Data.Appendful.Persistent
import Foobar.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} sr = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $ serverProcessSyncQuery [ServerThingUser ==. uid] serverMakeThing (makeServerThing uid) sr
