{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import Ledger hiding (txOutDatum, validatorHash)
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import qualified PlutusTx
import Text.Show (Show)

type SnapshotNumber = Integer

type Hash = BuiltinByteString

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [(Party, PubKeyHash)]
      }
  | Open
      { parties :: [(Party, PubKeyHash)]
      , utxoHash :: Hash
      }
  | Closed
      { snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      }
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Close
      { snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , signature :: [Signature]
      }
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer}
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
