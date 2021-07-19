{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.Party where

import Hydra.Prelude hiding (init)

import qualified PlutusTx
import PlutusTx.IsData
import Schema (FormSchema (..), ToSchema (..))

-- TODO(SN): Copied party + json instances for deserializing in 'init' endpoint
-- and we were struggling to define 'Lift' and 'IsData'

newtype Party = UnsafeParty Integer -- (VerKeyDSIGN MockDSIGN)
  deriving stock (Eq, Generic)
  deriving newtype (Show, Num)

PlutusTx.makeLift ''Party

instance ToJSON Party where
  toJSON (UnsafeParty i) = toJSON i

instance FromJSON Party where
  parseJSON = fmap fromInteger . parseJSON

instance ToSchema Party where
  toSchema = FormSchemaUnsupported "Party"

instance PlutusTx.IsData Party where
  toBuiltinData (UnsafeParty k) = toBuiltinData k

  fromBuiltinData = fmap fromInteger . fromBuiltinData

  unsafeFromBuiltinData = fromInteger . unsafeFromBuiltinData
