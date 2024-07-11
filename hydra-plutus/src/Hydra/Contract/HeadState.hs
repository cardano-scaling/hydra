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

-- | Sub-type for the open state-machine state.
data OpenDatum = OpenDatum
  { headId :: CurrencySymbol
  , parties :: [Party]
  , contestationPeriod :: ContestationPeriod
  , version :: SnapshotVersion
  -- ^ Spec: v
  , utxoHash :: Hash
  -- ^ Spec: η
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''OpenDatum

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , headId :: CurrencySymbol
      , seed :: TxOutRef
      }
  | Open OpenDatum
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

-- | Sub-type for close transition with auxiliary data as needed.
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

-- | Sub-type for contest transition with auxiliary data as needed.
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

-- | Sub-type for decrement transition with auxiliary data as needed.
data DecrementRedeemer = DecrementRedeemer
  { signature :: [Signature]
  -- ^ Spec: ξ
  , snapshotNumber :: SnapshotNumber
  -- ^ Spec: s
  , numberOfDecommitOutputs :: Integer
  -- ^ Spec: m
  }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''DecrementRedeemer

data Input
  = CollectCom
  | Decrement DecrementRedeemer
  | Close CloseRedeemer
  | Contest ContestRedeemer
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer, numberOfDecommitOutputs :: Integer}
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
