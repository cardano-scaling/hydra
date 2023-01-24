{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import Plutus.V2.Ledger.Api (CurrencySymbol, POSIXTime, PubKeyHash)
import qualified PlutusTx
import Text.Show (Show)

type SnapshotNumber = Integer

type Hash = BuiltinByteString

type Signature = BuiltinByteString

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , headId :: CurrencySymbol
      }
  | Open
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , utxoHash :: Hash
      , headId :: CurrencySymbol
      }
  | Closed
      { parties :: [Party]
      , snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , contestationDeadline :: POSIXTime
      , headId :: CurrencySymbol
      , contesters :: [PubKeyHash]
      }
  | Final
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Close
      { signature :: [Signature]
      }
  | Contest
      { signature :: [Signature]
      }
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer}
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
