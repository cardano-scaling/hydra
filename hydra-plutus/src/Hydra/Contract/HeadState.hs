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
  { headSeed :: TxOutRef
  -- ^ TODO: Spec?
  , headId :: CurrencySymbol
  -- ^ Spec: cid
  , parties :: [Party]
  -- ^ Spec: kH
  , contestationPeriod :: ContestationPeriod
  -- ^ Spec: T
  , version :: SnapshotVersion
  -- ^ Spec: v
  , accumulatorHash :: Hash
  -- ^ Digest of the accumulator hash for the last confirmed snapshot
  , headAdaOverhead :: Integer
  -- ^ Lovelace in the head UTxO not belonging to any L2 UTxO (min-UTxO overhead).
  -- Set once at init time and invariant for the head's lifetime.
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
  , contesters :: [PubKeyHash]
  -- ^ Spec: C
  , contestationDeadline :: POSIXTime
  -- ^ Spec: tfinal
  , accumulatorCommitment :: BuiltinBLS12_381_G1_Element
  -- ^ KZG commitment to the full UTxO set.
  , headAdaOverhead :: Integer
  -- ^ Lovelace in the head UTxO not belonging to any L2 UTxO (min-UTxO overhead).
  -- Propagated unchanged from OpenDatum via Close.
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''ClosedDatum

-- | Sub-type for intermediate partial fanout state. Carries only the fields
-- needed for subsequent partial fanout steps.
data FanoutProgressDatum = FanoutProgressDatum
  { headId :: CurrencySymbol
  , parties :: [Party]
  , contestationDeadline :: POSIXTime
  , accumulatorCommitment :: BuiltinBLS12_381_G1_Element
  , headAdaOverhead :: Integer
  -- ^ Lovelace in the head UTxO not belonging to any L2 UTxO. Propagated from ClosedDatum.
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''FanoutProgressDatum

-- | Extract the fields needed for partial fanout steps from a ClosedDatum.
-- Called both on-chain (in the validator dispatch) and off-chain (in tx building).
progressFromClosed :: ClosedDatum -> FanoutProgressDatum
progressFromClosed ClosedDatum{headId, parties, contestationDeadline, accumulatorCommitment, headAdaOverhead} =
  FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment, headAdaOverhead}
{-# INLINEABLE progressFromClosed #-}

data State
  = Open OpenDatum
  | Closed ClosedDatum
  | Final
  | FanoutProgress FanoutProgressDatum
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''State

-- | Sub-type for close transition with auxiliary data as needed.
data CloseRedeemer
  = -- | Initial snapshot is used to close.
    CloseInitial
  | -- | Any snapshot which doesn't contain anything to inc/decrement but snapshot number is higher than zero.
    CloseAny
      { signature :: [Signature]
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator hash
      }
  | -- | Closing snapshot refers to the current state version (pending inc/dec not yet applied)
    CloseUnused
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator hash
      }
  | -- | Closing snapshot refers to the previous state version (pending inc/dec already applied)
    CloseUsed
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator hash
      }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''CloseRedeemer

-- | Sub-type for contest transition with auxiliary data as needed.
data ContestRedeemer
  = -- | Contesting snapshot refers to the current state version (inc/dec not yet applied, or no pending action)
    ContestUnused
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator hash
      }
  | -- | Contesting snapshot refers to the previous state version (pending inc/dec already applied)
    ContestUsed
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator hash
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
  = Increment IncrementRedeemer
  | Decrement DecrementRedeemer
  | Close CloseRedeemer
  | Contest ContestRedeemer
  | Fanout
      { numberOfFanoutOutputs :: Integer
      , proof :: BuiltinBLS12_381_G1_Element
      , crsRef :: TxOutRef
      }
  | PartialFanout
      { numberOfPartialOutputs :: Integer
      , crsRef :: TxOutRef
      }
  | FinalPartialFanout
      { numberOfPartialOutputs :: Integer
      , proof :: BuiltinBLS12_381_G1_Element
      , crsRef :: TxOutRef
      }
  deriving stock (Generic, Show)

-- NOTE: The constructor indices here are load-bearing: the @deposit.ak@
-- validator reads the @Input@ redeemer's constructor index directly via
-- @builtin.un_constr_data@ to check that the head input is being spent with
-- the @Increment@ redeemer. Keep this list and @validators/deposit.ak@ in
-- sync.
PlutusTx.makeIsDataIndexed
  ''Input
  [ ('Increment, 0)
  , ('Decrement, 1)
  , ('Close, 2)
  , ('Contest, 3)
  , ('Fanout, 4)
  , ('PartialFanout, 5)
  , ('FinalPartialFanout, 6)
  ]
