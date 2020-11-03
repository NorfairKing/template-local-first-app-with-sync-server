{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Appendful ()
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeless ()
import Data.GenValidity.Text ()
import Foo.Bar.API.Data
import Foo.Bar.API.Server.Data.Gen ()

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncRequest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncResponse where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
