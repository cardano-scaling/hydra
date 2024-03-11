{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import PlutusLedgerApi.V2 (CurrencySymbol, POSIXTime, PubKeyHash, TxOutRef)
import PlutusTx qualified
import Text.Show (Show)

type SnapshotNumber = Integer

type Hash = BuiltinByteString

type Signature = BuiltinByteString

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , headId :: CurrencySymbol
      , seed :: TxOutRef
      }
  | Open
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , utxoHash :: Hash
      -- ^ Spec: η
      , headId :: CurrencySymbol
      , snapshotNumber :: SnapshotNumber
      }
  | Closed
      { parties :: [Party]
      , snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      -- ^ Spec: η
      , utxoToDecommitHash :: Hash
      -- ^ Spec: ηω
      , contestationDeadline :: POSIXTime
      , contestationPeriod :: ContestationPeriod
      , headId :: CurrencySymbol
      , contesters :: [PubKeyHash]
      }
  | Final
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Decrement
      { signature :: [Signature]
      , numberOfDecommitOutputs :: Integer
      }
  | Close
      { signature :: [Signature]
      }
  | Contest
      { signature :: [Signature]
      }
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer}
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
