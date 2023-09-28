{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Foo.Bar.Data.Thing where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import GHC.Generics (Generic)

data Thing = Thing
  { thingNumber :: !Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Thing)

instance Validity Thing

instance HasCodec Thing where
  codec =
    object "Thing" $
      Thing
        <$> requiredField "number" "thing number"
          .= thingNumber
