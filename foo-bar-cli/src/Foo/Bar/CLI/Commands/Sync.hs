module Foo.Bar.CLI.Commands.Sync where

import Data.Appendful.Persistent
import Foo.Bar.CLI.Commands.Import

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ clientMakeSyncRequestQuery clientMakeThing ClientThingServerId
  resp <- runClientOrDie cenv $ postSync fooBarClient token req
  runDB $ clientMergeSyncResponseQuery makeSyncedClientThing ClientThingServerId resp
