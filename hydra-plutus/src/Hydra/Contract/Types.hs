{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Types where

import Ledger (
  Datum (Datum),
  DatumHash,
  MonetaryPolicy,
  MonetaryPolicyHash,
  PubKeyHash,
  TxOut,
  TxOutRef,
  datumHash,
 )
import PlutusPrelude (Generic)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

data HydraState
  = Started
  | Initial [PubKeyHash] MonetaryPolicyHash
  | Open
  | Closed
  deriving stock (Prelude.Eq, Prelude.Show)

data HydraInput
  = Init [PubKeyHash] MonetaryPolicyHash
  | Commit PubKeyHash [(TxOutRef, TxOut)]
  | CollectCom
  | Close
  deriving (Generic)

newtype VerificationKey = VerificationKey
  { unverificationKey :: ByteString
  }
  deriving (Prelude.Eq, Show)

data HeadParameters = HeadParameters
  { verificationKeys :: [PubKeyHash]
  , monetaryPolicy :: MonetaryPolicy
  }
  deriving (Prelude.Eq, Show)

toDatumHash :: PlutusTx.IsData a => a -> DatumHash
toDatumHash = datumHash . Datum . PlutusTx.toData

PlutusTx.makeLift ''HydraState
PlutusTx.makeLift ''HydraInput

PlutusTx.unstableMakeIsData ''HydraState
PlutusTx.unstableMakeIsData ''HydraInput
