{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Server.Data.Gen where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password
import Foo.Bar.API.Server.Data
import Foo.Bar.Data.Gen ()
import Test.QuickCheck

instance GenValid Salt where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Pass where
  genValid = mkPass <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid PassHash where
  genValid = hashPassWithSalt <$> genValid <*> (mkPass <$> genValid)
  shrinkValid _ = [] -- No use

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerAppendfulThing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerMergelessThing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerMergefulThing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
