module Foobar.CLISpec (spec) where

import qualified Data.Text as T
import Foobar.API.Data
import Foobar.API.Server.Data
import Foobar.API.Server.TestUtils
import Foobar.CLI
import Path
import Path.IO
import Servant.Client
import System.Environment
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "Foobar CLI"
  $ it "'just works'"
  $ \cenv -> forAllValid $ \rf -> withSystemTempDir "foobar-cli" $ \tdir -> do
    setEnv "FOOBAR_SERVER_URL" $ showBaseUrl $ baseUrl cenv
    setEnv "FOOBAR_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
    setEnv "FOOBAR_PASSWORD" $ T.unpack $ registrationFormPassword rf
    dbFile <- resolveFile tdir "foobar-client.sqlite3"
    setEnv "FOOBAR_DATABASE" $ fromAbsFile dbFile
    let testFoobar args = withArgs args foobarCLI
    testFoobar ["register"]
    testFoobar ["login"]
    testFoobar ["sync"]
