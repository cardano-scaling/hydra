{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Handling of 'HostPreference' type we use in our API websockets server.
-- Unfortunatelly this type internals are not exposed by default so this module provides
-- needed instances and utilities to help remedy this situation.

module Hydra.API.Host
  ( HostPreference (..)
  , ipToHostPreference
  ) where

import Hydra.Prelude

import Data.Aeson
import Data.IP (IP (IPv4, IPv6))
import qualified Data.Streaming.Network.Internal as NI
import Network.Wai.Handler.Warp (HostPreference)
import qualified Data.Text as T

deriving instance Generic HostPreference

instance ToJSON HostPreference where
    toJSON = toJSON . hostPreferenceToText

instance FromJSON HostPreference where
    parseJSON = withText "HostPreference" $ \t ->
      case hostPreferenceFromText t of
        Left e -> fail e
        Right h -> return h

instance Arbitrary HostPreference where
  arbitrary = genericArbitrary

hostPreferenceToText :: HostPreference -> T.Text
hostPreferenceToText NI.HostAny = "*"
hostPreferenceToText NI.HostIPv4 = "*4"
hostPreferenceToText NI.HostIPv4Only = "!4"
hostPreferenceToText NI.HostIPv6 = "*6"
hostPreferenceToText NI.HostIPv6Only = "!6"
hostPreferenceToText (NI.Host s) = T.pack s

hostPreferenceFromText :: MonadFail m => T.Text -> m HostPreference
hostPreferenceFromText "*" = return NI.HostAny
hostPreferenceFromText "*4" = return NI.HostIPv4
hostPreferenceFromText "!4" = return NI.HostIPv4Only
hostPreferenceFromText "*6" = return NI.HostIPv6
hostPreferenceFromText "!6" = return NI.HostIPv6Only
hostPreferenceFromText s = pure $ fromString (T.unpack s)

ipToHostPreference :: IP -> HostPreference
ipToHostPreference = \case
  IPv4 _ -> NI.HostIPv4Only
  IPv6 _ -> NI.HostIPv6Only
