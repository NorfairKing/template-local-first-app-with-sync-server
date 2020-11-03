{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Data where

import Data.Aeson
import qualified Data.Appendful as Appendful
import Data.Functor.Contravariant
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeless as Mergeless
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Foo.Bar.API.Server.Data
import Foo.Bar.Client.Data
import Foo.Bar.Data
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm
  = RegistrationForm
      { registrationFormUsername :: Username,
        registrationFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance ToJSON RegistrationForm where
  toJSON RegistrationForm {..} =
    object
      [ "name" .= registrationFormUsername,
        "password" .= registrationFormPassword
      ]

instance FromJSON RegistrationForm where
  parseJSON =
    withObject "RegistrationForm" $ \o ->
      RegistrationForm <$> o .: "name" <*> o .: "password"

data LoginForm
  = LoginForm
      { loginFormUsername :: Username,
        loginFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o ->
    LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} =
    object
      [ "username" .= loginFormUsername,
        "password" .= loginFormPassword
      ]

data AuthCookie
  = AuthCookie
      { authCookieUsername :: Username
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data SyncRequest
  = SyncRequest
      { syncRequestAppendfulThingSyncRequest :: Appendful.SyncRequest ClientAppendfulThingId ServerAppendfulThingId Thing,
        syncRequestMergelessThingSyncRequest :: Mergeless.SyncRequest ClientMergelessThingId ServerMergelessThingId Thing,
        syncRequestMergefulThingSyncRequest :: Mergeful.SyncRequest ClientMergefulThingId ServerMergefulThingId Thing
      }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance FromJSON SyncRequest where
  parseJSON = withObject "SyncResponse" $ \o ->
    SyncRequest
      <$> o .: "appendful"
      <*> o .: "mergeless"
      <*> o .: "mergeful"

instance ToJSON SyncRequest where
  toJSON SyncRequest {..} =
    object
      [ "appendful" .= syncRequestAppendfulThingSyncRequest,
        "mergeless" .= syncRequestMergelessThingSyncRequest,
        "mergeful" .= syncRequestMergefulThingSyncRequest
      ]

data SyncResponse
  = SyncResponse
      { syncResponseAppendfulThingSyncResponse :: Appendful.SyncResponse ClientAppendfulThingId ServerAppendfulThingId Thing,
        syncResponseMergelessThingSyncResponse :: Mergeless.SyncResponse ClientMergelessThingId ServerMergelessThingId Thing,
        syncResponseMergefulThingSyncResponse :: Mergeful.SyncResponse ClientMergefulThingId ServerMergefulThingId Thing
      }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o ->
    SyncResponse
      <$> o .: "appendful"
      <*> o .: "mergeless"
      <*> o .: "mergeful"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object
      [ "appendful" .= syncResponseAppendfulThingSyncResponse,
        "mergeless" .= syncResponseMergelessThingSyncResponse,
        "mergeful" .= syncResponseMergefulThingSyncResponse
      ]

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
