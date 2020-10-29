module Foobar.CLI.Commands.Sync where

import Data.Appendful.Persistent
import Foobar.CLI.Commands.Import

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ clientMakeSyncRequestQuery clientMakeThing ClientThingServerId
  resp <- runClientOrDie cenv $ postSync foobarClient token req
  runDB $ clientMergeSyncResponseQuery makeSyncedClientThing ClientThingServerId resp
