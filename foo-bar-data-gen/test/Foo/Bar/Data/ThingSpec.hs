{-# LANGUAGE TypeApplications #-}

module Foo.Bar.Data.ThingSpec
  ( spec,
  )
where

import Foo.Bar.Data.Thing
import Foo.Bar.Data.Thing.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Thing
  jsonSpec @Thing
