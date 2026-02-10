{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import "plutus-tx" PlutusTx.Prelude

import "base" GHC.Generics (Generic)
import "base" Text.Show (Show)
import "hydra-plutus" Hydra.Data.ContestationPeriod (ContestationPeriod)
import "hydra-plutus" Hydra.Data.Party (Party)
import "plutus-ledger-api" PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, PubKeyHash, TxOutRef)
import "plutus-tx" PlutusTx qualified

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
  , alphaUTxOHash :: Hash
  , omegaUTxOHash :: Hash
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
  = -- | Initial snapshot is used to close.
    CloseInitial
  | -- | Any snapshot which doesn't contain anything to inc/decrement but snapshot number is higher than zero.
    CloseAny
      {signature :: [Signature]}
  | -- | Closing snapshot refers to the current state version
    CloseUnusedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  | -- | Closing snapshot refers to the previous state version
    CloseUsedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyDecommittedUTxOHash :: Hash
      -- ^ UTxO which was already decommitted ηω
      }
  | -- | Closing snapshot refers to the current state version
    CloseUnusedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyCommittedUTxOHash :: Hash
      -- ^ UTxO which was signed but not committed ηα
      }
  | -- | Closing snapshot refers to the previous state version
    CloseUsedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyCommittedUTxOHash :: Hash
      -- ^ UTxO which was already committed ηα
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
    ContestUsedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyDecommittedUTxOHash :: Hash
      -- ^ UTxO which was already decommitted ηω
      }
  | -- | Redeemer to use when the decommit was not yet observed but we closed the Head.
    ContestUnusedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  | -- | Redeemer to use when the commit was not yet observed but we closed the Head.
    ContestUnusedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyCommittedUTxOHash :: Hash
      -- ^ UTxO which was already committed ηα
      }
  | ContestUsedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''ContestRedeemer

-- | Sub-type for increment transition
data IncrementRedeemer = IncrementRedeemer
  { signature :: [Signature]
  , snapshotNumber :: SnapshotNumber
  , increment :: TxOutRef
  }
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
      , numberOfCommitOutputs :: Integer
      , numberOfDecommitOutputs :: Integer
      }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
