{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Contains the a stateful interface to transaction construction and observation.
--
-- It defines the 'ChainStateType tx' to be used in the 'Hydra.Chain.Direct'
-- layer and it's constituents.
module Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (PParams)
import GHC.IsList qualified as IsList
import Hydra.Cardano.Api (
  AssetId (..),
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey, VerificationKey),
  LedgerEra,
  NetworkId,
  PaymentKey,
  PolicyId,
  SlotNo (SlotNo),
  Tx,
  TxId,
  TxIn,
  TxIx (..),
  TxOut,
  UTxO,
  chainPointToSlotNo,
  fromCtxUTxOTxOut,
  fromPlutusTxOutRef,
  fromScriptData,
  isScriptTxOut,
  negateValue,
  toShelleyNetwork,
  txOutScriptData,
  txOutValue,
  utxoFromTx,
  pattern TxIn,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), IsChainState (..))
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  HeadId (..),
  HeadParameters (..),
  HeadSeed,
  Party,
  ScriptRegistry (..),
  Snapshot (..),
  SnapshotVersion (..),
  getSnapshot,
  headIdToPolicyId,
  headSeedToTxIn,
  partyFromChain,
  partyToChain,
  registryUTxO,
  txInToHeadSeed,
 )
import Hydra.Tx.Accumulator (HydraAccumulator)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), PointInTime, closeTx)
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.ContestationPeriod qualified as ContestationPeriod
import Hydra.Tx.Crypto (HydraKey, aggregate, generateSigningKey, sign)
import Hydra.Tx.Decrement (decrementTx)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx, observeDepositTxOut)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.DepositPeriod qualified as DepositPeriod
import Hydra.Tx.Fanout (fanoutTx, finalPartialFanoutTx, partialFanoutTx)
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (initTx)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Secret (Secret)
import Hydra.Tx.Utils (setIncrementalActionMaybe)

-- | A class for accessing the known 'UTxO' set in a type. This is useful to get
-- all the relevant UTxO for resolving transaction inputs.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States & transitions

-- | The chain state used by the Hydra.Chain.Direct implementation. It records
-- the actual 'ChainState' paired with a 'ChainSlot' (used to know up to which
-- point to rewind on rollbacks).
-- XXX: could move this into IsChainState and use UTxOType tx instead of ChainStateType tx
data ChainStateAt = ChainStateAt
  { spendableUTxO :: UTxO
  , recordedAt :: Maybe ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ChainStateAt where
  toCBOR = genericToCBOR

instance FromCBOR ChainStateAt where
  fromCBOR = genericFromCBOR

instance IsChainState Tx where
  type ChainPointType Tx = ChainPoint

  type ChainStateType Tx = ChainStateAt

  chainStatePoint ChainStateAt{recordedAt} =
    fromMaybe ChainPointAtGenesis recordedAt

  chainPointSlot = chainSlotFromPoint

-- | Get a generic 'ChainSlot' from a Cardano 'ChainPoint'. Slot 0 is used for
-- the genesis point.
chainSlotFromPoint :: ChainPoint -> ChainSlot
chainSlotFromPoint p =
  case chainPointToSlotNo p of
    Nothing -> ChainSlot 0
    Just (SlotNo s) -> ChainSlot $ fromIntegral s

-- | An enumeration of all possible on-chain states of a Hydra Head, where each
-- case stores the relevant information to construct & observe transactions to
-- other states.
data ChainState
  = -- | The idle state does not contain any head-specific information and exists to
    -- be used as a starting and terminal state.
    Idle
  | Open OpenState
  | Closed ClosedState
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO ChainState where
  getKnownUTxO :: ChainState -> UTxO
  getKnownUTxO = \case
    Idle -> mempty
    Open st -> getKnownUTxO st
    Closed st -> getKnownUTxO st

-- | Defines the starting state of the direct chain layer.
initialChainState :: ChainStateType Tx
initialChainState =
  ChainStateAt
    { spendableUTxO = mempty
    , recordedAt = Nothing
    }

-- | Read-only chain-specific data. This is different to 'HydraContext' as it
-- only contains data known to single peer.
data ChainContext = ChainContext
  { networkId :: NetworkId
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , scriptRegistry :: ScriptRegistry
  }
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO ChainContext where
  getKnownUTxO ChainContext{scriptRegistry} = registryUTxO scriptRegistry

data OpenState = OpenState
  { openUTxO :: UTxO
  , headId :: HeadId
  , seedTxIn :: TxIn
  }
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO OpenState where
  getKnownUTxO OpenState{openUTxO} =
    openUTxO

data ClosedState = ClosedState
  { closedUTxO :: UTxO
  , headId :: HeadId
  , seedTxIn :: TxIn
  , contestationDeadline :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO ClosedState where
  getKnownUTxO ClosedState{closedUTxO} =
    closedUTxO

-- * Constructing transactions

-- | Construct an init transaction given some general 'ChainContext', the
-- 'HeadParameters' and a seed 'TxIn' which will be spent.
initialize ::
  ChainContext ->
  PParams LedgerEra ->
  -- | Seed input.
  TxIn ->
  -- | Verification key hashes of all participants.
  [OnChainId] ->
  HeadParameters ->
  Tx
initialize ctx =
  initTx networkId
 where
  ChainContext{networkId} = ctx

data IncrementTxError
  = InvalidHeadSeedInIncrement {headSeed :: HeadSeed}
  | InvalidHeadIdInIncrement {headId :: HeadId}
  | CannotFindHeadOutputInIncrement
  | CannotFindDepositOutputInIncrement {depositTxId :: TxId}
  | SnapshotMissingIncrementUTxO
  | SnapshotIncrementUTxOIsNull
  | CannotObserveDraftedDeposit
  | CannotDecodeHeadDatumInIncrement
  deriving stock (Show)

-- | Construct a increment transaction spending the head and deposit outputs in given 'UTxO',
-- and producing single head output for pending 'utxoToCommit' of given 'Snapshot'.
increment ::
  ChainContext ->
  -- | Spendable UTxO containing head and deposit outputs
  UTxO ->
  (HeadSeed, HeadId) ->
  HeadParameters ->
  -- | Snapshot to increment with. Also names the deposit to claim.
  ConfirmedSnapshot Tx ->
  -- | Valid until, must be before deadline.
  SlotNo ->
  Either IncrementTxError Tx
increment ctx spendableUTxO (headSeed, headId) headParameters incrementingSnapshot upperValiditySlot = do
  seedTxIn <- headSeedToTxIn headSeed ?> InvalidHeadSeedInIncrement{headSeed}
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInIncrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputInIncrement
  -- NOTE: the deposit is taken from the snapshot rather than passed in. The
  -- increment validator recomputes the signed commit hash from the deposit input
  -- this transaction spends, so a transaction spending any deposit other than the
  -- one bound into this snapshot cannot validate. Deriving it here makes the two
  -- impossible to disagree.
  depositTxId <- snapshotDepositTxId ?> SnapshotMissingIncrementUTxO
  -- NOTE: resolve the exact output, not just the transaction id.
  -- 'Hydra.Contract.Head.checkIncrement' requires the claimed deposit to be its
  -- transaction's first output, so accepting any index here could build a
  -- transaction that cannot validate.
  (depositedIn, depositedOut) <-
    UTxO.findWithKey
      ( \txin txout ->
          txin == TxIn depositTxId (TxIx 0) && isScriptTxOut depositValidatorScript txout
      )
      spendableUTxO
      ?> CannotFindDepositOutputInIncrement{depositTxId}
  case utxoToCommit of
    Nothing ->
      Left SnapshotMissingIncrementUTxO
    Just deposit
      | UTxO.null deposit ->
          Left SnapshotIncrementUTxOIsNull
      | otherwise ->
          Right $
            incrementTx
              scriptRegistry
              ownVerificationKey
              (seedTxIn, headId)
              headParameters
              headUTxO
              sn
              (UTxO.singleton depositedIn depositedOut)
              upperValiditySlot
              sigs
 where
  Snapshot{utxoToCommit, depositTxId = snapshotDepositTxId} = sn

  (sn, sigs) =
    case incrementingSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      _ -> (getSnapshot incrementingSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

-- | Build an increment transaction claiming a drafted (not yet submitted)
-- deposit transaction against the current head output. The incrementing
-- snapshot is based on the given current confirmed snapshot, as the next
-- snapshot which would commit the drafted deposit. The result can never
-- validate on chain (for an 'InitialSnapshot' base the multi-signature is
-- fabricated, otherwise the current snapshot's signatures do not cover the
-- fabricated snapshot), but is byte-accurate in every component that matters
-- for size estimation: script witnesses, datum layout, one 64-byte signature
-- per party in the redeemer, and the merged head output value. The head seed,
-- parties and periods are decoded from the current head output's inline datum.
dryRunIncrementTx ::
  ChainContext ->
  -- | Spendable UTxO containing the current head output.
  UTxO ->
  HeadId ->
  -- | Current confirmed snapshot, basis for the incrementing snapshot.
  ConfirmedSnapshot Tx ->
  -- | Drafted (unbalanced) deposit transaction.
  Tx ->
  -- | Upper validity slot.
  SlotNo ->
  Either IncrementTxError Tx
dryRunIncrementTx ctx spendableUTxO headId currentSnapshot depositDraftTx upperValiditySlot = do
  DepositObservation{deposited, depositTxId} <-
    observeDepositTx networkId depositDraftTx ?> CannotObserveDraftedDeposit
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInIncrement{headId}
  (_, headOut) <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputInIncrement
  (headSeed, headParameters) <- decodeOpenDatum headOut
  let HeadParameters{parties} = headParameters
      Snapshot{version, number, utxo, accumulator} = getSnapshot currentSnapshot
      snapshot =
        Snapshot
          { headId
          , version
          , number = number + 1
          , confirmed = []
          , utxo
          , utxoToCommit = Just deposited
          , utxoToDecommit = Nothing
          , depositTxId = Just depositTxId
          , -- Only the constant-size hash of the accumulator ends up in the
            -- transaction.
            accumulator
          }
      signatures = case currentSnapshot of
        -- Real multi-signature of the right multiplicity; that it does not
        -- cover the fabricated snapshot is irrelevant, the dry-run is never
        -- verified.
        ConfirmedSnapshot{signatures = sigs} -> sigs
        -- The initial snapshot carries no signatures, so fabricate one
        -- never-verified, but byte-identical, signature per party.
        _ -> aggregate (sign dummySigningKey snapshot <$ parties)
  increment
    ctx
    -- Inject the not-yet-submitted deposit output into the spendable set.
    (spendableUTxO <> utxoFromTx depositDraftTx)
    (headSeed, headId)
    headParameters
    ConfirmedSnapshot{snapshot, signatures}
    upperValiditySlot
 where
  dummySigningKey = generateSigningKey "hydra-dry-run-increment"

  ChainContext{networkId} = ctx

-- | Decode head seed and parameters from the inline datum of a head output.
decodeOpenDatum :: TxOut CtxUTxO -> Either IncrementTxError (HeadSeed, HeadParameters)
decodeOpenDatum headOut =
  case fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headOut) of
    Just (Head.Open Head.OpenDatum{headSeed, parties = onChainParties, contestationPeriod, depositPeriod}) -> do
      parties <- traverse partyFromChain onChainParties ?> CannotDecodeHeadDatumInIncrement
      pure
        ( txInToHeadSeed (fromPlutusTxOutRef headSeed)
        , HeadParameters
            { contestationPeriod = ContestationPeriod.fromChain contestationPeriod
            , depositPeriod = DepositPeriod.fromChain depositPeriod
            , parties
            }
        )
    _ -> Left CannotDecodeHeadDatumInIncrement

-- | Possible errors when trying to construct decrement tx
data DecrementTxError
  = InvalidHeadSeedInDecrement {headSeed :: HeadSeed}
  | InvalidHeadIdInDecrement {headId :: HeadId}
  | CannotFindHeadOutputInDecrement
  | DecrementValueNegative
  | SnapshotDecrementUTxOIsNull
  deriving stock (Show)

-- | Construct a decrement transaction spending the head output in given 'UTxO',
-- and producing outputs for all pending 'utxoToDecommit' of given 'Snapshot'.
decrement ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  (HeadSeed, HeadId) ->
  HeadParameters ->
  -- | Snapshot to decrement with.
  ConfirmedSnapshot Tx ->
  Either DecrementTxError Tx
decrement ctx spendableUTxO (headSeed, headId) headParameters decrementingSnapshot = do
  seedTxIn <- headSeedToTxIn headSeed ?> InvalidHeadSeedInDecrement{headSeed}
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInDecrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO@(_, headOut) <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputInDecrement
  let balance = txOutValue headOut <> negateValue decommitValue
  when (isNegative balance) $
    Left DecrementValueNegative
  Right $
    decrementTx
      scriptRegistry
      ownVerificationKey
      (seedTxIn, headId)
      headParameters
      headUTxO
      sn
      sigs
 where
  decommitValue = UTxO.totalValue $ fromMaybe mempty $ utxoToDecommit sn

  isNegative = any ((< 0) . snd) . IsList.toList

  (sn, sigs) =
    case decrementingSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      -- XXX: This way of retrofitting an 'InitialSnapshot' into a Snapshot +
      -- Signatures indicates we might want to simplify 'ConfirmedSnapshot' into
      -- a product directly.
      _ -> (getSnapshot decrementingSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

data CloseTxError
  = InvalidHeadIdInClose {headId :: HeadId}
  | CannotFindHeadOutputToClose
  | BothCommitAndDecommitInClose
  deriving stock (Show)

data RecoverTxError
  = InvalidHeadIdInRecover {headId :: HeadId}
  | CannotFindDepositOutputToRecover {depositTxId :: TxId}
  | CannotFindDepositedOutputToRecover {depositedTxId :: TxId}
  deriving stock (Show)

-- | Construct a recover transaction spending the deposit output
-- and producing outputs the user initially deposited.
recover ::
  ChainContext ->
  HeadId ->
  -- | Deposit TxId
  TxId ->
  -- | Spendable UTxO
  UTxO ->
  SlotNo ->
  Either RecoverTxError Tx
recover ctx headId depositedTxId spendableUTxO lowerValiditySlot = do
  -- NOTE: resolve the exact output, not just the transaction id. 'recoverTx'
  -- spends @TxIn depositedTxId (TxIx 0)@, so accepting any index here would
  -- inspect one output and then spend a different one.
  (_, depositedOut) <-
    UTxO.findWithKey
      ( \txin txout ->
          txin == TxIn depositedTxId (TxIx 0) && isScriptTxOut depositValidatorScript txout
      )
      spendableUTxO
      ?> CannotFindDepositOutputToRecover{depositTxId = depositedTxId}
  (headId', deposited, _deadline) <-
    observeDepositTxOut (toShelleyNetwork networkId) depositedOut
      ?> CannotFindDepositedOutputToRecover{depositedTxId = depositedTxId}
  if headId /= headId'
    then Left InvalidHeadIdInRecover{headId}
    else Right $ recoverTx depositedTxId deposited lowerValiditySlot
 where
  ChainContext{networkId} = ctx

-- | Construct a close transaction spending the head output in given 'UTxO',
-- head parameters, and a confirmed snapshot. NOTE: Lower and upper bound slot
-- difference should not exceed contestation period.
close ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Head id to close.
  HeadId ->
  -- | Parameters of the head to close.
  HeadParameters ->
  -- | Last known version of the open head. NOTE: We deliberately require a
  -- 'SnapshotVersion' to be passed in, even though it could be extracted from the
  -- open head output in the spendable UTxO, to stay consistent with the way
  -- parameters are handled.
  SnapshotVersion ->
  -- | Snapshot to close with.
  ConfirmedSnapshot Tx ->
  -- | 'Tx' validity lower bound
  SlotNo ->
  -- | 'Tx' validity upper bound
  PointInTime ->
  Either CloseTxError Tx
close ctx spendableUTxO headId HeadParameters{parties, contestationPeriod, depositPeriod} openVersion confirmedSnapshot startSlotNo pointInTime = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInClose{headId}
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputToClose
  let openThreadOutput =
        OpenThreadOutput
          { openThreadUTxO = headUTxO
          , openContestationPeriod = ContestationPeriod.toChain contestationPeriod
          , openDepositPeriod = DepositPeriod.toChain depositPeriod
          , openParties = partyToChain <$> parties
          }

  incrementalAction <- setIncrementalActionMaybe utxoToCommit utxoToDecommit ?> BothCommitAndDecommitInClose
  pure $ closeTx scriptRegistry ownVerificationKey headId openVersion confirmedSnapshot startSlotNo pointInTime openThreadOutput incrementalAction
 where
  Snapshot{utxoToCommit, utxoToDecommit} = getSnapshot confirmedSnapshot

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

data ContestTxError
  = InvalidHeadIdInContest {headId :: HeadId}
  | CannotFindHeadOutputToContest
  | MissingHeadDatumInContest
  | MissingHeadRedeemerInContest
  | WrongDatumInContest
  | FailedToConvertFromScriptDataInContest
  deriving stock (Show)

-- | Construct a contest transaction based on the 'ClosedState' and a confirmed
-- snapshot. The given 'PointInTime' will be used as an upper validity bound and
-- needs to be before the deadline.
contest ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  ContestationPeriod ->
  -- | Last known version of the open head. NOTE: We deliberately require a
  -- 'SnapshotVersion' to be passed in, even though it could be extracted from the
  -- open head output in the spendable UTxO, to stay consistent with the way
  -- parameters are handled.
  SnapshotVersion ->
  -- | Snapshot to contest with.
  ConfirmedSnapshot Tx ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  Either ContestTxError Tx
contest ctx spendableUTxO headId contestationPeriod openVersion contestingSnapshot pointInTime = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInContest{headId}
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputToContest
  closedThreadOutput <- extractProgressDatum headUTxO
  pure $ contestTx scriptRegistry ownVerificationKey headId contestationPeriod openVersion sn sigs pointInTime closedThreadOutput
 where
  extractProgressDatum headUTxO@(_, headOutput) = do
    headDatum <- txOutScriptData (fromCtxUTxOTxOut headOutput) ?> MissingHeadDatumInContest
    datum <- fromScriptData headDatum ?> FailedToConvertFromScriptDataInContest

    case datum of
      Head.Closed Head.ClosedDatum{contesters, parties, contestationDeadline, headAdaOverhead, depositPeriod} -> do
        let closedThreadUTxO = headUTxO
            closedParties = parties
            closedContestationDeadline = contestationDeadline
            closedContesters = contesters
        pure $
          ClosedThreadOutput
            { closedThreadUTxO
            , closedParties
            , closedContestationDeadline
            , closedContesters
            , closedHeadAdaOverhead = headAdaOverhead
            , closedDepositPeriod = depositPeriod
            }
      _ -> Left WrongDatumInContest

  (sn, sigs) =
    case contestingSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      -- XXX: This way of retrofitting an 'InitialSnapshot' into a Snapshot +
      -- Signatures indicates we might want to simplify 'ConfirmedSnapshot' into
      -- a product directly.
      _ -> (getSnapshot contestingSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

data FanoutTxError
  = CannotFindHeadOutputToFanout
  | MissingHeadDatumInFanout
  | WrongDatumInFanout
  | FailedToConvertFromScriptDataInFanout
  | BothCommitAndDecommitInFanout
  | FailedToCreateFanoutProof Text
  deriving stock (Show)

-- | Construct a fanout transaction based on the 'ClosedState' and off-chain
-- agreed 'UTxO' set to fan out.
fanout ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Snapshot UTxO to fanout
  UTxO ->
  -- | Snapshot UTxO to commit to fanout
  Maybe UTxO ->
  -- | Snapshot UTxO to decommit to fanout
  Maybe UTxO ->
  -- | Full snapshot UTxO for accumulator (matches closed datum)
  UTxO ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Either FanoutTxError Tx
fanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit utxoForProof deadlineSlotNo = do
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO)
      ?> CannotFindHeadOutputToFanout
  closedThreadUTxO <- extractProgressDatum headUTxO
  _ <- setIncrementalActionMaybe utxoToCommit utxoToDecommit ?> BothCommitAndDecommitInFanout
  fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit utxoForProof closedThreadUTxO deadlineSlotNo headTokenScript
    & first FailedToCreateFanoutProof
 where
  headTokenScript = mkHeadTokenScript seedTxIn

  ChainContext{scriptRegistry} = ctx

  extractProgressDatum :: (TxIn, TxOut CtxUTxO) -> Either FanoutTxError (TxIn, TxOut CtxUTxO)
  extractProgressDatum headUTxO@(_, headOutput) = do
    headDatum <-
      txOutScriptData (fromCtxUTxOTxOut headOutput) ?> MissingHeadDatumInFanout
    datum <-
      fromScriptData headDatum ?> FailedToConvertFromScriptDataInFanout

    case datum of
      Head.Closed{} -> pure headUTxO
      _ -> Left WrongDatumInFanout

-- | Errors that can occur when constructing partial or final-partial fanout transactions.
data PartialFanoutError
  = CannotFindHeadOutput
  | MissingHeadDatum
  | WrongDatum
  | FailedToConvertFromScriptData
  | -- | The on-chain accumulator no longer matches the UTxOs we want to
    -- distribute. This happens when another node already posted a partial
    -- fanout and the chain state moved forward.
    StaleChainState
  | -- | Membership proof generation failed (e.g. subset element not in accumulator
    -- or CRS too short). Indicates a programming error in the caller.
    CannotCreateProof Text
  deriving stock (Eq, Show)

-- | Everything a partial fanout needs that does not depend on the chunk size.
-- Chunk sizes are searched for by trying candidate transactions, so this is
-- prepared once per step and reused for each candidate.
data PartialFanoutPlan = PartialFanoutPlan
  { headUTxO :: (TxIn, TxOut CtxUTxO)
  , progressDatum :: Head.FanoutProgressDatum
  , fullAccumulator :: HydraAccumulator
  -- ^ Accumulator over the proof UTxO, already verified against the on-chain
  -- commitment. Pre-settled elements (committed to by the accumulator but never
  -- distributed, e.g. a decommit paid out before close) are in here and stay in
  -- the remaining accumulator, because a step only removes what it distributes.
  , orderedRemaining :: [(TxIn, TxOut CtxUTxO)]
  -- ^ 'remainingUTxO' in the order chunks are taken from.
  }

-- | Read the head output and verify the on-chain accumulator, yielding a plan
-- to build partial fanout transactions from.
--
-- Handles both the first step (Closed → FanoutProgress) and intermediate steps
-- (FanoutProgress → FanoutProgress) by detecting the current datum type.
preparePartialFanout ::
  -- | Spendable UTxO containing head output
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | UTxO used to verify the on-chain accumulator commitment. For the first fanout
  -- step this is utxoForProof (the snapshot's full set, including any decommit UTxOs
  -- that may already have been removed from the head by a DecrementTx). For subsequent
  -- FanoutProgress steps it is the not-yet-distributed set plus any pre-settled
  -- elements, which the remaining set only equals when the whole of it is being
  -- distributed: a user selection is a sub-multiset of it, matched by output
  -- content rather than by 'TxIn'.
  UTxO ->
  -- | Remaining UTxOs to distribute
  UTxO ->
  Either PartialFanoutError PartialFanoutPlan
preparePartialFanout spendableUTxO seedTxIn proofUTxO remainingUTxO = do
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO)
      ?> CannotFindHeadOutput
  headState <- readHeadState headUTxO
  progressDatum <- case headState of
    Head.Closed closedDatum -> pure (Head.progressFromClosed closedDatum)
    Head.FanoutProgress d -> pure d
    _ -> Left WrongDatum
  fullAccumulator <- buildAndVerifyAccumulator progressDatum proofUTxO
  pure
    PartialFanoutPlan
      { headUTxO
      , progressDatum
      , fullAccumulator
      , orderedRemaining = UTxO.toList remainingUTxO
      }

-- | Construct a partial fanout transaction distributing the first 'chunkSize'
-- UTxOs of the plan's remaining set; the rest become the new remaining set.
--
-- The remaining accumulator is the plan's verified one minus the outputs this
-- transaction distributes, rather than rebuilt from scratch. Removing exactly
-- the distributed outputs is what the on-chain split identity @A = P_K * A'@
-- checks, so it holds by construction whenever those outputs are in @A@.
partialFanoutFromPlan ::
  ChainContext ->
  PartialFanoutPlan ->
  -- | Number of UTxOs to distribute in this step
  Int ->
  -- | Contestation deadline as SlotNo
  SlotNo ->
  Either PartialFanoutError Tx
partialFanoutFromPlan ctx plan chunkSize deadlineSlotNo = do
  let utxoToDistribute = UTxO.fromList (take chunkSize orderedRemaining)
  when (UTxO.null utxoToDistribute) $ Left (CannotCreateProof "utxoToDistribute must not be empty")
  let remainingAccumulator = Accumulator.removeOutputs @Tx fullAccumulator utxoToDistribute
  pure $ partialFanoutTx scriptRegistry utxoToDistribute headUTxO deadlineSlotNo progressDatum remainingAccumulator
 where
  PartialFanoutPlan{headUTxO, progressDatum, fullAccumulator, orderedRemaining} = plan

  ChainContext{scriptRegistry} = ctx

-- | Construct a partial fanout transaction that distributes a subset of UTxOs.
--
-- 'preparePartialFanout' followed by 'partialFanoutFromPlan'. Callers building
-- more than one candidate for the same head should use those directly and keep
-- the plan, which is where the per-step accumulator work lives.
partialFanout ::
  ChainContext ->
  -- | Spendable UTxO containing head output
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Number of UTxOs to distribute in this step
  Int ->
  -- | UTxO used to verify the on-chain accumulator commitment
  UTxO ->
  -- | Remaining UTxOs to distribute (will be split into distribute + new remaining)
  UTxO ->
  -- | Contestation deadline as SlotNo
  SlotNo ->
  Either PartialFanoutError Tx
partialFanout ctx spendableUTxO seedTxIn chunkSize proofUTxO remainingUTxO deadlineSlotNo = do
  plan <- preparePartialFanout spendableUTxO seedTxIn proofUTxO remainingUTxO
  partialFanoutFromPlan ctx plan chunkSize deadlineSlotNo

-- | Construct the final partial fanout transaction that distributes all remaining
-- UTxOs and burns all head tokens. Reads FanoutProgressDatum from the head output.
finalPartialFanout ::
  ChainContext ->
  -- | Spendable UTxO containing head output
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | All remaining UTxOs to distribute
  UTxO ->
  -- | Pre-settled UTxO: elements in the snapshot accumulator that are never
  -- distributed (e.g. a decommit UTxO paid out before close). mempty in normal case.
  UTxO ->
  -- | Contestation deadline as SlotNo
  SlotNo ->
  Either PartialFanoutError Tx
finalPartialFanout ctx spendableUTxO seedTxIn utxoToDistribute presettledUTxO deadlineSlotNo = do
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO)
      ?> CannotFindHeadOutput
  headState <- readHeadState headUTxO
  progressDatum <- case headState of
    Head.FanoutProgress d -> pure d
    _ -> Left WrongDatum
  -- The accumulator verified against the datum here is the one the membership
  -- proof is built against, rather than a second build over the same set.
  fullAccumulator <- buildAndVerifyAccumulator progressDatum (utxoToDistribute <> presettledUTxO)
  first CannotCreateProof $
    finalPartialFanoutTx
      scriptRegistry
      utxoToDistribute
      fullAccumulator
      headUTxO
      deadlineSlotNo
      headTokenScript
 where
  headTokenScript = mkHeadTokenScript seedTxIn
  ChainContext{scriptRegistry} = ctx

-- | Read and decode the head state from a head script output.
readHeadState :: (TxIn, TxOut CtxUTxO) -> Either PartialFanoutError Head.State
readHeadState (_, headOutput) = do
  headDatum <- txOutScriptData (fromCtxUTxOTxOut headOutput) ?> MissingHeadDatum
  fromScriptData headDatum ?> FailedToConvertFromScriptData

-- | Build an accumulator from the given UTxO and verify its commitment matches
-- the one in the on-chain datum. Returns the accumulator for reuse by the caller.
-- Fails with 'StaleChainState' if the commitments differ.
buildAndVerifyAccumulator ::
  Head.FanoutProgressDatum ->
  UTxO ->
  Either PartialFanoutError HydraAccumulator
buildAndVerifyAccumulator progressDatum utxo = do
  let acc = Accumulator.buildFromUTxO @Tx utxo
      Head.FanoutProgressDatum{accumulatorCommitment = onChain} = progressDatum
  unless (Accumulator.getAccumulatorCommitment acc == onChain) $
    Left StaleChainState
  pure acc

-- * Helpers

utxoOfThisHead :: PolicyId -> UTxO -> UTxO
utxoOfThisHead policy = UTxO.filter hasHeadToken
 where
  hasHeadToken =
    isJust . find isHeadToken . IsList.toList . txOutValue

  isHeadToken (assetId, quantity) =
    case assetId of
      AdaAssetId -> False
      AssetId pid _ -> pid == policy && quantity == 1

-- * Generators

-- | Define some 'global' context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
--
-- Do not use this in production code, but only for generating test data.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxHydraSigningKeys :: [Secret (SigningKey HydraKey)]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: ContestationPeriod
  , ctxDepositPeriod :: DepositPeriod
  , ctxScriptRegistry :: ScriptRegistry
  }
  deriving stock (Show)
