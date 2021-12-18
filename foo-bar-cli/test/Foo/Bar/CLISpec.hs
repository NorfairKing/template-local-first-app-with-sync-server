module Foo.Bar.CLISpec (spec) where

import qualified Data.Text as T
import Foo.Bar.API.Data
import Foo.Bar.API.Server.Data
import Foo.Bar.API.Server.TestUtils
import Foo.Bar.CLI
import Path
import Path.IO
import Servant.Client
import System.Environment
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
  describe "Foo.Bar CLI" $
    it "'just works'" $
      \cenv -> forAllValid $ \rf -> withSystemTempDir "foo-bar-cli" $ \tdir -> do
        dbFile <- resolveFile tdir "foo-bar-client.sqlite3"
        let testFooBar args = do
              setEnv "FOO_BAR_SERVER_URL" $ showBaseUrl $ baseUrl cenv
              setEnv "FOO_BAR_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
              setEnv "FOO_BAR_PASSWORD" $ T.unpack $ registrationFormPassword rf
              setEnv "FOO_BAR_DATABASE" $ fromAbsFile dbFile
              withArgs args fooBarCLI
        testFooBar ["register"]
        testFooBar ["login"]
        testFooBar ["sync"]
