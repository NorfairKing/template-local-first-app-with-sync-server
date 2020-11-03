{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.CLI.Commands.Sync where

import Data.Appendful.Persistent
import Foo.Bar.CLI.Commands.Import

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  syncRequestThingSyncRequest <- runDB $ clientMakeSyncRequestQuery clientMakeThing ClientAppendThingServerId
  let req = SyncRequest {..}
  SyncResponse {..} <- runClientOrDie cenv $ postSync fooBarClient token req
  runDB $ clientMergeSyncResponseQuery makeSyncedClientAppendThing ClientAppendThingServerId syncResponseThingSyncResponse
