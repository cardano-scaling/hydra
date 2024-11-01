{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, PubKeyHash, TxOutRef)
import PlutusTx qualified
import Text.Show (Show)

type SnapshotNumber = Integer

type SnapshotVersion = Integer

type Hash = BuiltinByteString

type Signature = BuiltinByteString

-- | Sub-type for the open state-machine state.
data OpenDatum = OpenDatum
  { headId :: CurrencySymbol
  -- ^ Spec: cid
  , parties :: [Party]
  -- ^ Spec: kH
  , contestationPeriod :: ContestationPeriod
  -- ^ Spec: T
  , version :: SnapshotVersion
  -- ^ Spec: v
  , utxoHash :: Hash
  -- ^ Spec: η
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''OpenDatum

-- | Sub-type for the closed state-machine state.
data ClosedDatum = ClosedDatum
  { headId :: CurrencySymbol
  -- ^ Spec: cid
  , parties :: [Party]
  -- ^ Spec: kH
  , contestationPeriod :: ContestationPeriod
  -- ^ Spec: T
  , version :: SnapshotVersion
  -- ^ Spec: v
  , snapshotNumber :: SnapshotNumber
  -- ^ Spec: s
  , utxoHash :: Hash
  -- ^ Spec: η. Digest of snapshotted UTxO
  , deltaUTxOHash :: Hash
  -- ^ Spec: ηΔ. Digest of UTxO still to be distributed
  , contesters :: [PubKeyHash]
  -- ^ Spec: C
  , contestationDeadline :: POSIXTime
  -- ^ Spec: tfinal
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''ClosedDatum

data State
  = Initial
      { contestationPeriod :: ContestationPeriod
      , parties :: [Party]
      , headId :: CurrencySymbol
      , seed :: TxOutRef
      }
  | Open OpenDatum
  | Closed ClosedDatum
  | Final
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

-- | Sub-type for close transition with auxiliary data as needed.
data CloseRedeemer
  = -- | Intial snapshot is used to close.
    CloseInitial
  | -- | Closing snapshot refers to the current state version
    CloseUnused
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  | -- | Closing snapshot refers to the previous state version
    CloseUsed
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

-- | Sub-type for increment transition
-- TODO: add more fields as needed.
data IncrementRedeemer = IncrementRedeemer
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''IncrementRedeemer

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
  | Increment IncrementRedeemer
  | Decrement DecrementRedeemer
  | Close CloseRedeemer
  | Contest ContestRedeemer
  | Abort
  | Fanout
      { numberOfFanoutOutputs :: Integer
      -- ^ Spec: m
      , numberOfDecommitOutputs :: Integer
      -- ^ Spec: n
      }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
