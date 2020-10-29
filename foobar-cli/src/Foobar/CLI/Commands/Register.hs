{-# LANGUAGE RecordWildCards #-}

module Foobar.CLI.Commands.Register where

import Foobar.CLI.Commands.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister foobarClient rf
  pure ()
