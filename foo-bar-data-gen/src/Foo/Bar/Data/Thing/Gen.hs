{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.Data.Thing.Gen where

import Data.GenValidity
import Foo.Bar.Data.Thing

instance GenValid Thing where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
