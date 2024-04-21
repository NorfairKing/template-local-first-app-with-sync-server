{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Server.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Foo.Bar.API.Server.Data
import Foo.Bar.Data.Gen ()

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
