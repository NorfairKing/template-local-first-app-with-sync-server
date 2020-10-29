{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foobar.Data.Thing.Gen where

import Data.GenValidity
import Foobar.Data.Thing

instance GenValid Thing where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
