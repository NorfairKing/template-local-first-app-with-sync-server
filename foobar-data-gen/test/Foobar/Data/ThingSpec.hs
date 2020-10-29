{-# LANGUAGE TypeApplications #-}

module Foobar.Data.ThingSpec
  ( spec,
  )
where

import Foobar.Data.Thing
import Foobar.Data.Thing.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  genValidSpec @Thing
