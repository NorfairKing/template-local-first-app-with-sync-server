module Foo.Bar.CLI.Commands.Login where

import Foo.Bar.CLI.Commands.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()
