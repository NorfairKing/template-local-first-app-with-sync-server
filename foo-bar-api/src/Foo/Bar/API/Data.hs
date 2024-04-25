{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Data where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import qualified Data.Appendful as Appendful
import Data.Functor.Contravariant
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeless as Mergeless
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql as DB
import Foo.Bar.API.Server.Data
import Foo.Bar.Client.Data
import Foo.Bar.Data
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RegistrationForm)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance HasCodec RegistrationForm where
  codec =
    object "RegistrationForm" $
      RegistrationForm
        <$> requiredField "name" "user name"
          .= registrationFormUsername
        <*> requiredField "password" "password"
          .= registrationFormPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "user name"
          .= loginFormUsername
        <*> requiredField "password" "password"
          .= loginFormPassword

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data SyncRequest = SyncRequest
  { syncRequestAppendfulThingSyncRequest :: Appendful.SyncRequest ClientAppendfulThingId ServerAppendfulThingId Thing,
    syncRequestMergelessThingSyncRequest :: Mergeless.SyncRequest ClientMergelessThingId ServerMergelessThingId Thing,
    syncRequestMergefulThingSyncRequest :: Mergeful.SyncRequest ClientMergefulThingId ServerMergefulThingId Thing
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncRequest)

instance Validity SyncRequest

instance HasCodec SyncRequest where
  codec =
    object "SyncResponse" $
      SyncRequest
        <$> requiredField "appendful" "appendful fields"
          .= syncRequestAppendfulThingSyncRequest
        <*> requiredField "mergeless" "mergeless items"
          .= syncRequestMergelessThingSyncRequest
        <*> requiredField "mergeful" "mergeful items"
          .= syncRequestMergefulThingSyncRequest

data SyncResponse = SyncResponse
  { syncResponseAppendfulThingSyncResponse :: Appendful.SyncResponse ClientAppendfulThingId ServerAppendfulThingId Thing,
    syncResponseMergelessThingSyncResponse :: Mergeless.SyncResponse ClientMergelessThingId ServerMergelessThingId Thing,
    syncResponseMergefulThingSyncResponse :: Mergeful.SyncResponse ClientMergefulThingId ServerMergefulThingId Thing
  }
  deriving (FromJSON, ToJSON) via (Autodocodec SyncResponse)

instance HasCodec SyncResponse where
  codec =
    object "SyncResponse" $
      SyncResponse
        <$> requiredField "appendful" "appendful fields"
          .= syncResponseAppendfulThingSyncResponse
        <*> requiredField "mergeless" "mergeless items"
          .= syncResponseMergelessThingSyncResponse
        <*> requiredField "mergeful" "mergeful items"
          .= syncResponseMergefulThingSyncResponse

instance (ToBackendKey SqlBackend a) => HasCodec (Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
