{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Types where

import Ledger (
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  PubKeyHash,
  TxOutRef,
  datumHash,
 )
import PlutusPrelude (Generic)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

data HydraState
  = Started
  | Initial [PubKeyHash]
  | Open
  | Closed
  deriving stock (Prelude.Eq, Prelude.Show)

data HydraInput
  = Init HeadParameters
  | Commit PubKeyHash [TxOutRef]
  | CollectCom
  | Close
  deriving (Generic)

newtype VerificationKey = VerificationKey
  { unverificationKey :: ByteString
  }
  deriving (Prelude.Eq, Show)

data HeadParameters = HeadParameters
  { verificationKeys :: [PubKeyHash]
  , currencyId :: CurrencySymbol
  }
  deriving (Prelude.Eq, Show)

toDatumHash :: PlutusTx.IsData a => a -> DatumHash
toDatumHash = datumHash . Datum . PlutusTx.toData

PlutusTx.makeLift ''HydraState
PlutusTx.makeLift ''HydraInput
PlutusTx.makeLift ''HeadParameters

PlutusTx.unstableMakeIsData ''HydraState
PlutusTx.unstableMakeIsData ''HydraInput
PlutusTx.unstableMakeIsData ''HeadParameters
