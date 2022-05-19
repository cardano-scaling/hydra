{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import Plutus.V1.Ledger.Api (POSIXTime)
import Plutus.V1.Ledger.Crypto (Signature)
import qualified PlutusTx
import Text.Show (Show)

type SnapshotNumber = Integer

type Hash = BuiltinByteString

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      }
  | Open
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , utxoHash :: Hash
      }
  | Closed
      { parties :: [Party]
      , snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , contestationDeadline :: POSIXTime
      }
  | Final
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Close
      { snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , signature :: [Signature]
      }
  | Contest
      { snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , signature :: [Signature]
      }
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer}
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
