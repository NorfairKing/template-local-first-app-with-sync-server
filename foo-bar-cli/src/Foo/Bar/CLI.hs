{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.CLI
  ( fooBarCLI,
  )
where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Foo.Bar.CLI.Commands as Commands
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import System.FileLock

fooBarCLI :: IO ()
fooBarCLI = do
  Instructions disp Settings {..} <- getInstructions
  mCenv <- forM settingBaseUrl $ \burl -> do
    man <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ mkClientEnv man burl
  ensureDir $ parent settingDbFile
  -- Block until locking succeeds
  withFileLock (fromAbsFile settingDbFile ++ ".lock") Exclusive $ \_ ->
    runStderrLoggingT $
      filterLogger (\_ ll -> ll >= settingLogLevel) $
        withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
          _ <- runSqlPool (runMigrationQuiet clientMigration) pool
          let env =
                Env
                  { envClientEnv = mCenv,
                    envUsername = settingUsername,
                    envPassword = settingPassword,
                    envConnectionPool = pool
                  }
          liftIO $ runReaderT (dispatch disp) env

dispatch :: Dispatch -> C ()
dispatch = \case
  DispatchRegister -> Commands.register
  DispatchLogin -> Commands.login
  DispatchSync -> Commands.sync
