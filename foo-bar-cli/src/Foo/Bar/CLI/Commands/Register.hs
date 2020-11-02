{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.CLI.Commands.Register where

import Foo.Bar.CLI.Commands.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister fooBarClient rf
  pure ()
