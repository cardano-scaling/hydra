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

type SnapshotVersion = Integer

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
      , version :: SnapshotVersion
      }
  | Closed
      { parties :: [Party]
      , snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      -- ^ Spec: η
      , utxoToDecommitHash :: Hash
      -- ^ Spec: ηΔ
      , contestationDeadline :: POSIXTime
      , contestationPeriod :: ContestationPeriod
      , headId :: CurrencySymbol
      , contesters :: [PubKeyHash]
      , version :: SnapshotVersion
      }
  | Final
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

-- | Type of close and auxiliary data if needed.
data CloseRedeemer
  = -- | Intial snapshot is used to close.
    CloseInitial
  | -- | Closing snapshot refers to the current state version
    CloseCurrent
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  | -- | Closing snapshot refers to the previous state version
    CloseOutdated
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyDecommittedUTxOHash :: Hash
      -- ^ UTxO which was already decommitted ηω
      }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''CloseRedeemer

-- | Type of contestation and auxiliary data if needed.
data ContestRedeemer
  = -- | Contesting snapshot refers to the current state version
    ContestCurrent
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  | -- | Contesting snapshot refers to the previous state version
    ContestOutdated
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyDecommittedUTxOHash :: Hash
      -- ^ UTxO which was already decommitted ηω
      }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''ContestRedeemer

data Input
  = CollectCom
  | Decrement {signature :: [Signature], numberOfDecommitOutputs :: Integer}
  | Close CloseRedeemer
  | Contest ContestRedeemer
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer, numberOfDecommitOutputs :: Integer}
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
