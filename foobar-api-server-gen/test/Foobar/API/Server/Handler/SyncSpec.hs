module Foobar.API.Server.Handler.SyncSpec (spec) where

import Foobar.API
import Foobar.API.Data.Gen ()
import Foobar.API.Server.TestUtils
import Foobar.Client
import Foobar.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "PostSync"
  $ it "does not crash"
  $ \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token -> do
    resp <- testClientOrErr cenv $ postSync foobarClient token req
    shouldBeValid resp
