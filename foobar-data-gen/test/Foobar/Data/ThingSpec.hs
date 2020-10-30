{-# LANGUAGE TypeApplications #-}

module Foobar.Data.ThingSpec
  ( spec,
  )
where

import Foobar.Data.Thing
import Foobar.Data.Thing.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Thing
  jsonSpecOnValid @Thing
