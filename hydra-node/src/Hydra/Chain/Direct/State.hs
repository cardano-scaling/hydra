{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Contains the a stateful interface to transaction construction and observation.
--
-- It defines the 'ChainStateType tx' to be used in the 'Hydra.Chain.Direct'
-- layer and it's constituents.
module Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import GHC.IsList qualified as IsList
import Hydra.Cardano.Api (
  AssetId (..),
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey, VerificationKey),
  NetworkId,
  PaymentKey,
  PolicyId,
  SlotNo (SlotNo),
  Tx,
  TxId,
  TxIn,
  TxOut,
  UTxO,
  chainPointToSlotNo,
  fromCtxUTxOTxOut,
  fromScriptData,
  isScriptTxOut,
  mkTxIn,
  negateValue,
  toCtxUTxOTxOut,
  toShelleyNetwork,
  txOutScriptData,
  txOutValue,
  txOuts',
  pattern TxIn,
 )
import Hydra.Chain (
  OnChainTx (..),
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), IsChainState (..))
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Ledger.Cardano (adjustUTxO)
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
  deriveParty,
  getSnapshot,
  headIdToPolicyId,
  headSeedToTxIn,
  partyToChain,
  registryUTxO,
 )
import Hydra.Tx.Close (OpenThreadOutput (..), PointInTime, closeTx)
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.ContestationPeriod qualified as ContestationPeriod
import Hydra.Tx.Crypto (HydraKey)
import Hydra.Tx.Decrement (decrementTx)
import Hydra.Tx.Deposit (observeDepositTxOut)
import Hydra.Tx.Fanout (fanoutTx)
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (initTx)
import Hydra.Tx.Observe (
  CloseObservation (..),
  InitObservation (..),
  NotAnInitReason (..),
  observeCloseTx,
  observeInitTx,
 )
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Utils (setIncrementalActionMaybe, verificationKeyToOnChainId)

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
  deriving stock (Show)

-- | Construct a increment transaction spending the head and deposit outputs in given 'UTxO',
-- and producing single head output for pending 'utxoToCommit' of given 'Snapshot'.
increment ::
  ChainContext ->
  -- | Spendable UTxO containing head and deposit outputs
  UTxO ->
  (HeadSeed, HeadId) ->
  HeadParameters ->
  -- | Snapshot to increment with.
  ConfirmedSnapshot Tx ->
  -- | Deposited TxId
  TxId ->
  -- | Valid until, must be before deadline.
  SlotNo ->
  Either IncrementTxError Tx
increment ctx spendableUTxO (headSeed, headId) headParameters incrementingSnapshot depositTxId upperValiditySlot = do
  seedTxIn <- headSeedToTxIn headSeed ?> InvalidHeadSeedInIncrement{headSeed}
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInIncrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputInIncrement
  (depositedIn, depositedOut) <-
    UTxO.findWithKey
      ( \(TxIn txid _) txout ->
          isScriptTxOut depositValidatorScript txout && txid == depositTxId
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
  Snapshot{utxoToCommit} = sn

  (sn, sigs) =
    case incrementingSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      _ -> (getSnapshot incrementingSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

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
  (_, depositedOut) <-
    UTxO.findWithKey
      ( \(TxIn txid _) txout ->
          isScriptTxOut depositValidatorScript txout && txid == depositedTxId
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
close ctx spendableUTxO headId HeadParameters{parties, contestationPeriod} openVersion confirmedSnapshot startSlotNo pointInTime = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInClose{headId}
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputToClose
  let openThreadOutput =
        OpenThreadOutput
          { openThreadUTxO = headUTxO
          , openContestationPeriod = ContestationPeriod.toChain contestationPeriod
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
  | BothCommitAndDecommitInContest
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
  closedThreadOutput <- checkHeadDatum headUTxO
  incrementalAction <- setIncrementalActionMaybe utxoToCommit utxoToDecommit ?> BothCommitAndDecommitInContest
  pure $ contestTx scriptRegistry ownVerificationKey headId contestationPeriod openVersion sn sigs pointInTime closedThreadOutput incrementalAction
 where
  Snapshot{utxoToCommit, utxoToDecommit} = sn
  checkHeadDatum headUTxO@(_, headOutput) = do
    headDatum <- txOutScriptData (fromCtxUTxOTxOut headOutput) ?> MissingHeadDatumInContest
    datum <- fromScriptData headDatum ?> FailedToConvertFromScriptDataInContest

    case datum of
      Head.Closed Head.ClosedDatum{contesters, parties, contestationDeadline} -> do
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
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Either FanoutTxError Tx
fanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit deadlineSlotNo = do
  headUTxO <-
    UTxO.find (isScriptTxOut Head.validatorScript) (utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO)
      ?> CannotFindHeadOutputToFanout
  closedThreadUTxO <- checkHeadDatum headUTxO
  _ <- setIncrementalActionMaybe utxoToCommit utxoToDecommit ?> BothCommitAndDecommitInFanout
  pure $ fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit closedThreadUTxO deadlineSlotNo headTokenScript
 where
  headTokenScript = mkHeadTokenScript seedTxIn

  ChainContext{scriptRegistry} = ctx

  checkHeadDatum :: (TxIn, TxOut CtxUTxO) -> Either FanoutTxError (TxIn, TxOut CtxUTxO)
  checkHeadDatum headUTxO@(_, headOutput) = do
    headDatum <-
      txOutScriptData (fromCtxUTxOTxOut headOutput) ?> MissingHeadDatumInFanout
    datum <-
      fromScriptData headDatum ?> FailedToConvertFromScriptDataInFanout

    case datum of
      Head.Closed{} -> pure headUTxO
      _ -> Left WrongDatumInFanout

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

-- * Observing Transitions

-- ** IdleState transitions

-- TODO: This function is not really used anymore (only from
-- 'unsafeObserveInit'). In general, most functions here are actually not used
-- from the "production code", but only to generate test cases and benchmarks.

-- | Observe an init transition using a 'InitialState' and 'observeInitTx'.
observeInit ::
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  Either NotAnInitReason (OnChainTx Tx, OpenState)
observeInit _ctx _allVerificationKeys tx = do
  observation <- observeInitTx tx
  headOut <- head <$> nonEmpty (txOuts' tx) ?> NoHeadOutput
  let headUTxO = UTxO.singleton (mkTxIn tx 0) (toCtxUTxOTxOut headOut)
  pure (toEvent observation, toState headUTxO observation)
 where
  toEvent :: InitObservation -> OnChainTx Tx
  toEvent InitObservation{headParameters, headId, headSeed, participants} =
    OnInitTx{headId, headSeed, headParameters, participants}

  toState openUTxO InitObservation{headId, headSeed} =
    OpenState
      { openUTxO
      , headId
      , seedTxIn = fromJust $ headSeedToTxIn headSeed
      }

-- ** OpenState transitions

-- | Observe a close transition using a 'OpenState' and 'observeCloseTx'.
-- This function checks the head id and ignores if not relevant.
observeClose ::
  OpenState ->
  Tx ->
  Maybe (OnChainTx Tx, ClosedState)
observeClose st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCloseTx utxo tx
  let CloseObservation{headId = closeObservationHeadId, snapshotNumber, contestationDeadline} = observation
  guard (headId == closeObservationHeadId)
  let event =
        OnCloseTx
          { headId = closeObservationHeadId
          , snapshotNumber
          , contestationDeadline
          }
  let st' =
        ClosedState
          { closedUTxO = adjustUTxO tx utxo
          , headId
          , seedTxIn
          , contestationDeadline
          }
  pure (event, st')
 where
  OpenState
    { headId
    , seedTxIn
    } = st

-- * Generators

-- ** Warning zone

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
  , ctxHydraSigningKeys :: [SigningKey HydraKey]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: ContestationPeriod
  , ctxScriptRegistry :: ScriptRegistry
  }
  deriving stock (Show)

ctxParties :: HydraContext -> [Party]
ctxParties = fmap deriveParty . ctxHydraSigningKeys

ctxParticipants :: HydraContext -> [OnChainId]
ctxParticipants = map verificationKeyToOnChainId . ctxVerificationKeys

ctxHeadParameters ::
  HydraContext ->
  HeadParameters
ctxHeadParameters ctx@HydraContext{ctxContestationPeriod} =
  HeadParameters ctxContestationPeriod (ctxParties ctx)

-- ** Danger zone

unsafeIncrement ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable 'UTxO'
  UTxO ->
  (HeadSeed, HeadId) ->
  HeadParameters ->
  ConfirmedSnapshot Tx ->
  TxId ->
  SlotNo ->
  Tx
unsafeIncrement ctx spendableUTxO headId parameters incrementingSnapshot depositedTxId slotNo =
  either (error . show) id $ increment ctx spendableUTxO headId parameters incrementingSnapshot depositedTxId slotNo

unsafeDecrement ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable 'UTxO'
  UTxO ->
  (HeadSeed, HeadId) ->
  HeadParameters ->
  ConfirmedSnapshot Tx ->
  Tx
unsafeDecrement ctx spendableUTxO headId parameters decrementingSnapshot =
  either (error . show) id $ decrement ctx spendableUTxO headId parameters decrementingSnapshot

-- | Unsafe version of 'close' that throws an error if the transaction fails to build.
unsafeClose ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  HeadParameters ->
  SnapshotVersion ->
  ConfirmedSnapshot Tx ->
  SlotNo ->
  PointInTime ->
  Tx
unsafeClose ctx spendableUTxO headId headParameters openVersion confirmedSnapshot startSlotNo pointInTime =
  either (error . show) id $ close ctx spendableUTxO headId headParameters openVersion confirmedSnapshot startSlotNo pointInTime

-- | Unsafe version of 'contest' that throws an error if the transaction fails to build.
unsafeContest ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  ContestationPeriod ->
  SnapshotVersion ->
  ConfirmedSnapshot Tx ->
  PointInTime ->
  Tx
unsafeContest ctx spendableUTxO headId contestationPeriod openVersion contestingSnapshot pointInTime =
  either (error . show) id $ contest ctx spendableUTxO headId contestationPeriod openVersion contestingSnapshot pointInTime

unsafeFanout ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Snapshot UTxO to fanout
  UTxO ->
  -- | Snapshot commit UTxO to fanout
  Maybe UTxO ->
  -- | Snapshot decommit UTxO to fanout
  Maybe UTxO ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Tx
unsafeFanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit deadlineSlotNo =
  either (error . show) id $ fanout ctx spendableUTxO seedTxIn utxo utxoToCommit utxoToDecommit deadlineSlotNo

unsafeObserveInit ::
  HasCallStack =>
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  OpenState
unsafeObserveInit cctx txInit allVerificationKeys =
  case observeInit cctx txInit allVerificationKeys of
    Left err -> error $ "Did not observe an init tx: " <> show err
    Right st -> snd st
