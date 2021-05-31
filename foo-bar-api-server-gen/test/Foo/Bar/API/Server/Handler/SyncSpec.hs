module Foo.Bar.API.Server.Handler.SyncSpec (spec) where

import Foo.Bar.API
import Foo.Bar.API.Data.Gen ()
import Foo.Bar.API.Server.TestUtils
import Foo.Bar.Client
import Foo.Bar.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
  describe "PostSync" $
    it "does not crash" $
      \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token -> do
        _ <- testClientOrErr cenv $ postSync fooBarClient token req
        pure ()
