{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.HeadState where

import PlutusTx.Prelude

import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, PubKeyHash, TokenName, TxOutRef)
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
  , utxoHash :: Hash
  -- ^ Spec: η
  , accumulatorHash :: Hash
  -- ^ Digest of the accumulator ηA for the last confirmed snapshot
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
  , accumulatorCommitment :: BuiltinBLS12_381_G1_Element
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''ClosedDatum

-- | Sub-type for intermediate partial fanout state. Carries only the fields
-- needed for subsequent partial fanout steps, dropping the snapshot-specific
-- fields that are no longer relevant after the first partial fanout.
data FanoutProgressDatum = FanoutProgressDatum
  { headId :: CurrencySymbol
  , parties :: [Party]
  , contestationDeadline :: POSIXTime
  , accumulatorCommitment :: BuiltinBLS12_381_G1_Element
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''FanoutProgressDatum

-- | Extract the fields needed for partial fanout steps from a ClosedDatum.
-- Called both on-chain (in the validator dispatch) and off-chain (in tx building).
progressFromClosed :: ClosedDatum -> FanoutProgressDatum
progressFromClosed ClosedDatum{headId, parties, contestationDeadline, accumulatorCommitment} =
  FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment}
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
      }
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
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator ηA
      }
  | -- | Contesting snapshot refers to the previous state version
    ContestUsedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyDecommittedUTxOHash :: Hash
      -- ^ UTxO which was already decommitted ηω
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator ηA
      }
  | -- | Redeemer to use when the decommit was not yet observed but we closed the Head.
    ContestUnusedDec
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator ηA
      }
  | -- | Redeemer to use when the commit was not yet observed but we closed the Head.
    ContestUnusedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , alreadyCommittedUTxOHash :: Hash
      -- ^ UTxO which was already committed ηα
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator ηA
      }
  | ContestUsedInc
      { signature :: [Signature]
      -- ^ Multi-signature of a snapshot ξ
      , accumulatorHash :: Hash
      -- ^ Digest of the accumulator ηA
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

-- | Sub-type for dynamic-head-participants transitions (issue #1813). Phase 1
-- supports only 'RemovePartyOC'; Phase 2 will add 'AddPartyOC' and additional
-- variants for arbitrary parameter changes will follow. Constructor tags
-- are stable on chain: never re-order existing variants.
data OnChainParameterUpdate
  = -- | Remove a current 'Party' from the head. The 'TokenName' is the asset
    -- name of the leaving party's participation token; the validator burns
    -- exactly that PT.
    RemovePartyOC Party TokenName
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''OnChainParameterUpdate

-- | Sub-type for the 'UpdateParameters' transition (issue #1813). Carries the
-- multi-signature over the snapshot whose 'parameterUpdate' authorizes the
-- change, the snapshot number, and the on-chain representation of the update.
data UpdateParametersRedeemer = UpdateParametersRedeemer
  { signature :: [Signature]
  -- ^ Spec: ξ
  , snapshotNumber :: SnapshotNumber
  -- ^ Spec: s
  , parameterUpdate :: OnChainParameterUpdate
  -- ^ Spec: pω
  }
  deriving stock (Show, Generic)

PlutusTx.unstableMakeIsData ''UpdateParametersRedeemer

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
  | PartialFanout
      { numberOfPartialOutputs :: Integer
      , crsRef :: TxOutRef
      }
  | FinalPartialFanout
      { numberOfPartialOutputs :: Integer
      , proof :: BuiltinBLS12_381_G1_Element
      , crsRef :: TxOutRef
      }
  | -- | Apply a multi-signed parameter change (e.g., a party leaving) in the
    -- open state. See 'OnChainParameterUpdate'.
    UpdateParameters UpdateParametersRedeemer
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Input
