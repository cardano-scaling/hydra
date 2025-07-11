{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Contains the a stateful interface to transaction construction and observation.
--
-- It defines the 'ChainStateType tx' to be used in the 'Hydra.Chain.Direct'
-- layer and it's constituents.
module Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import GHC.IsList qualified as IsList
import Hydra.Cardano.Api (
  AsType (AsHash),
  AssetId (..),
  AssetName (AssetName),
  BlockHeader (..),
  ChainPoint (..),
  CtxUTxO,
  HasTypeProxy (..),
  Key (SigningKey, VerificationKey, verificationKeyHash),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  PolicyId,
  Quantity (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  SlotNo (SlotNo),
  Tx,
  TxId,
  TxIn,
  TxOut,
  UTxO,
  UTxO' (UTxO),
  Value,
  chainPointToSlotNo,
  deserialiseFromRawBytes,
  fromCtxUTxOTxOut,
  fromScriptData,
  genTxIn,
  getTxBody,
  getTxId,
  isScriptTxOut,
  mkTxIn,
  modifyTxOutValue,
  negateValue,
  selectAsset,
  toCtxUTxOTxOut,
  toShelleyNetwork,
  txIns',
  txOutScriptData,
  txOutValue,
  txOuts',
  txSpendingUTxO,
  pattern ByronAddressInEra,
  pattern ShelleyAddressInEra,
  pattern TxIn,
  pattern TxOut,
 )
import Hydra.Chain (
  OnChainTx (..),
  PostTxError (..),
  maxMainnetLovelace,
  maximumNumberOfParties,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), IsChainState (..))
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Ledger.Cardano.Evaluate (genPointInTimeBefore, genValidityBoundsFromContestationPeriod, slotLength, systemStart)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus (commitValidatorScript, depositValidatorScript, initialValidatorScript)
import Hydra.Tx (
  CommitBlueprintTx (..),
  ConfirmedSnapshot (..),
  HeadId (..),
  HeadParameters (..),
  Party,
  ScriptRegistry (..),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion (..),
  deriveParty,
  getSnapshot,
  headIdToPolicyId,
  headSeedToTxIn,
  mkSimpleBlueprintTx,
  partyToChain,
  registryUTxO,
  utxoFromTx,
 )
import Hydra.Tx.Abort (AbortTxError (..), abortTx)
import Hydra.Tx.Close (OpenThreadOutput (..), PointInTime, closeTx)
import Hydra.Tx.CollectCom (UTxOHash, collectComTx)
import Hydra.Tx.Commit (commitTx)
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.ContestationPeriod qualified as ContestationPeriod
import Hydra.Tx.Crypto (HydraKey)
import Hydra.Tx.Decrement (decrementTx)
import Hydra.Tx.Deposit (DepositObservation (..), depositTx, observeDepositTx, observeDepositTxOut)
import Hydra.Tx.Fanout (fanoutTx)
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (initTx)
import Hydra.Tx.Observe (
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  InitObservation (..),
  NotAnInitReason (..),
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
  observeInitTx,
 )
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Snapshot (genConfirmedSnapshot)
import Hydra.Tx.Utils (setIncrementalActionMaybe, splitUTxO, verificationKeyToOnChainId)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (
  genOneUTxOFor,
  genScriptRegistry,
  genTxOut,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genVerificationKey,
 )
import Test.QuickCheck (choose, chooseEnum, elements, frequency, oneof, suchThat, vector)

-- | A class for accessing the known 'UTxO' set in a type. This is useful to get
-- all the relevant UTxO for resolving transaction inputs.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States & transitions

-- | The chain state used by the Hydra.Chain.Direct implementation. It records
-- the actual 'ChainState' paired with a 'ChainSlot' (used to know up to which
-- point to rewind on rollbacks).
data ChainStateAt = ChainStateAt
  { spendableUTxO :: UTxO
  , recordedAt :: Maybe ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ChainPoint where
  toCBOR = \case
    ChainPointAtGenesis -> toCBOR ("ChainPointAtGenesis" :: Text)
    ChainPoint slotNo bHash ->
      toCBOR ("ChainPoint" :: Text)
        <> toCBOR slotNo
        <> toCBOR (serialiseToRawBytes bHash)

instance FromCBOR ChainPoint where
  fromCBOR =
    fromCBOR >>= \case
      ("ChainPointAtGenesis" :: Text) -> pure ChainPointAtGenesis
      ("ChainPoint" :: Text) ->
        ChainPoint
          <$> fromCBOR
          <*> ( fromCBOR >>= \bs ->
                  case deserialiseFromRawBytes (AsHash (proxyToAsType (Proxy :: Proxy BlockHeader))) bs of
                    Left e -> fail (show e)
                    Right h -> pure h
              )
      msg -> fail $ show msg <> " is not a proper CBOR-encoded ChainPoint"

instance ToCBOR ChainStateAt where
  toCBOR ChainStateAt{spendableUTxO, recordedAt} =
    toCBOR ("ChainStateAt" :: Text) <> toCBOR spendableUTxO <> toCBOR recordedAt

instance FromCBOR ChainStateAt where
  fromCBOR =
    fromCBOR >>= \case
      ("ChainStateAt" :: Text) -> ChainStateAt <$> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance Arbitrary ChainStateAt where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance IsChainState Tx where
  type ChainStateType Tx = ChainStateAt

  chainStateSlot ChainStateAt{recordedAt} =
    maybe (ChainSlot 0) chainSlotFromPoint recordedAt

-- | Get a generic 'ChainSlot' from a Cardano 'ChainPoint'. Slot 0 is used for
-- the genesis point.
chainSlotFromPoint :: ChainPoint -> ChainSlot
chainSlotFromPoint p =
  case chainPointToSlotNo p of
    Nothing -> ChainSlot 0
    Just (SlotNo s) -> ChainSlot $ fromIntegral s

-- | A definition of all transitions between 'ChainState's. Enumerable and
-- bounded to be used as labels for checking coverage.
data ChainTransition
  = Init
  | Abort
  | Commit
  | Collect
  | Increment
  | Decrement
  | Close
  | Contest
  | Fanout
  deriving stock (Eq, Show, Enum, Bounded)

-- | An enumeration of all possible on-chain states of a Hydra Head, where each
-- case stores the relevant information to construct & observe transactions to
-- other states.
data ChainState
  = -- | The idle state does not contain any head-specific information and exists to
    -- be used as a starting and terminal state.
    Idle
  | Initial InitialState
  | Open OpenState
  | Closed ClosedState
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO ChainState where
  getKnownUTxO :: ChainState -> UTxO
  getKnownUTxO = \case
    Idle -> mempty
    Initial st -> getKnownUTxO st
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

instance Arbitrary ChainContext where
  arbitrary = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    ownVerificationKey <- genVerificationKey
    otherParties <- choose (1, maximumNumberOfParties) >>= \n -> replicateM n arbitrary
    ownParty <- elements otherParties
    scriptRegistry <- genScriptRegistry
    pure
      ChainContext
        { networkId
        , ownVerificationKey
        , ownParty
        , scriptRegistry
        }

-- | Representation of the Head output after an Init transaction.
data InitialThreadOutput = InitialThreadOutput
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initialContestationPeriod :: OnChain.ContestationPeriod
  , initialParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)

data InitialState = InitialState
  { initialThreadOutput :: InitialThreadOutput
  , initialInitials :: [(TxIn, TxOut CtxUTxO)]
  , initialCommits :: [(TxIn, TxOut CtxUTxO)]
  , headId :: HeadId
  , seedTxIn :: TxIn
  }
  deriving stock (Eq, Show, Generic)

instance HasKnownUTxO InitialState where
  getKnownUTxO st =
    UTxO $
      Map.fromList $
        initialThreadUTxO : initialCommits <> initialInitials
   where
    InitialState
      { initialThreadOutput = InitialThreadOutput{initialThreadUTxO}
      , initialInitials
      , initialCommits
      } = st

data OpenState = OpenState
  { openUTxO :: UTxO
  , headId :: HeadId
  , seedTxIn :: TxIn
  , openUtxoHash :: UTxOHash
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary OpenState where
  arbitrary = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStOpen ctx

  shrink = genericShrink

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

-- | Construct a commit transaction based on known, spendable UTxO and some
-- arbitrary UTxOs to commit. This does look for "our initial output" to spend
-- and check the given 'UTxO' to be compatible. Hence, this function does fail
-- if already committed or if the head is not initializing.
--
-- NOTE: This version of 'commit' does only commit outputs which are held by
-- payment keys. For a variant which supports committing scripts, see `commit'`.
commit ::
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  -- | 'UTxO' to commit. All outputs are assumed to be owned by public keys
  UTxO ->
  Either (PostTxError Tx) Tx
commit ctx headId spendableUTxO lookupUTxO =
  let blueprintTx = txSpendingUTxO lookupUTxO
   in commit' ctx headId spendableUTxO CommitBlueprintTx{lookupUTxO, blueprintTx}

-- | Construct a commit transaction based on known, spendable UTxO and some
-- user UTxO inputs to commit. This does look for "our initial output" to spend
-- and check the given 'UTxO' to be compatible. Hence, this function does fail
-- if already committed or if the head is not initializing.
--
-- NOTE: A simpler variant only supporting pubkey outputs is 'commit'.
commit' ::
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  CommitBlueprintTx Tx ->
  Either (PostTxError Tx) Tx
commit' ctx headId spendableUTxO commitBlueprintTx = do
  pid <- headIdToPolicyId headId ?> InvalidHeadId{headId}
  (i, o) <- ownInitial pid ?> CannotFindOwnInitial{knownUTxO = spendableUTxO}
  rejectByronAddress lookupUTxO
  rejectMoreThanMainnetLimit networkId lookupUTxO
  pure $ commitTx networkId scriptRegistry headId ownParty commitBlueprintTx (i, o, vkh)
 where
  CommitBlueprintTx{lookupUTxO} = commitBlueprintTx

  ChainContext{networkId, ownParty, scriptRegistry, ownVerificationKey} = ctx

  vkh = verificationKeyHash ownVerificationKey

  ownInitial pid =
    UTxO.find (hasMatchingPT pid . txOutValue) spendableUTxO

  hasMatchingPT pid val =
    selectAsset val (AssetId pid (AssetName (serialiseToRawBytes vkh))) == 1

rejectByronAddress :: UTxO -> Either (PostTxError Tx) ()
rejectByronAddress u = do
  forM_ (UTxO.txOutputs u) $ \case
    (TxOut (ByronAddressInEra addr) _ _ _) ->
      Left (UnsupportedLegacyOutput addr)
    (TxOut ShelleyAddressInEra{} _ _ _) ->
      Right ()

-- Rejects outputs with more than 'maxMainnetLovelace' lovelace on mainnet
-- NOTE: Remove this limit once we have more experiments on mainnet.
rejectMoreThanMainnetLimit :: NetworkId -> UTxO -> Either (PostTxError Tx) ()
rejectMoreThanMainnetLimit network u = do
  when (network == Mainnet && lovelaceAmt > maxMainnetLovelace) $
    Left $
      CommittedTooMuchADAForMainnet lovelaceAmt maxMainnetLovelace
 where
  lovelaceAmt = UTxO.totalLovelace u

-- | Construct a abort transaction based on known, spendable UTxO. This function
-- looks for head, initial and commit outputs to spend and it will fail if we
-- can't find the head output.
abort ::
  ChainContext ->
  -- | Seed TxIn
  TxIn ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Committed UTxOs to reimburse.
  UTxO ->
  Either AbortTxError Tx
abort ctx seedTxIn spendableUTxO committedUTxO = do
  headUTxO <-
    maybe (Left CannotFindHeadOutputToAbort) pure $
      UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead'

  abortTx committedUTxO scriptRegistry ownVerificationKey headUTxO headTokenScript initials commits
 where
  utxoOfThisHead' = utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO

  initials =
    UTxO.toMap $ UTxO.filter (isScriptTxOut initialValidatorScript) utxoOfThisHead'

  commits =
    UTxO.toMap $ UTxO.filter (isScriptTxOut commitValidatorScript) utxoOfThisHead'

  headTokenScript = mkHeadTokenScript seedTxIn

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

data CollectTxError
  = InvalidHeadIdInCollect {headId :: HeadId}
  | CannotFindHeadOutputToCollect
  deriving stock (Show)

-- | Construct a collect transaction based on known, spendable UTxO. This
-- function looks for head output and commit outputs to spend and it will fail
-- if we can't find the head output.
collect ::
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  Either CollectTxError Tx
collect ctx headId headParameters utxoToCollect spendableUTxO = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInCollect{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputToCollect
  let commits = UTxO.toMap $ UTxO.filter (isScriptTxOut commitValidatorScript) utxoOfThisHead'
  pure $
    collectComTx networkId scriptRegistry ownVerificationKey headId headParameters headUTxO commits utxoToCollect
 where
  ChainContext{networkId, ownVerificationKey, scriptRegistry} = ctx

data IncrementTxError
  = InvalidHeadIdInIncrement {headId :: HeadId}
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
  HeadId ->
  HeadParameters ->
  -- | Snapshot to increment with.
  ConfirmedSnapshot Tx ->
  -- | Deposited TxId
  TxId ->
  -- | Valid until, must be before deadline.
  SlotNo ->
  Either IncrementTxError Tx
increment ctx spendableUTxO headId headParameters incrementingSnapshot depositTxId upperValiditySlot = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInIncrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputInIncrement
  (depositedIn, depositedOut) <-
    UTxO.findBy
      ( \(TxIn txid _, txout) ->
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
      | otherwise -> Right $ incrementTx scriptRegistry ownVerificationKey headId headParameters headUTxO sn (UTxO.singleton depositedIn depositedOut) upperValiditySlot sigs
 where
  Snapshot{utxoToCommit} = sn

  (sn, sigs) =
    case incrementingSnapshot of
      ConfirmedSnapshot{snapshot, signatures} -> (snapshot, signatures)
      _ -> (getSnapshot incrementingSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

-- | Possible errors when trying to construct decrement tx
data DecrementTxError
  = InvalidHeadIdInDecrement {headId :: HeadId}
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
  HeadId ->
  HeadParameters ->
  -- | Snapshot to decrement with.
  ConfirmedSnapshot Tx ->
  Either DecrementTxError Tx
decrement ctx spendableUTxO headId headParameters decrementingSnapshot = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInDecrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO@(_, headOut) <- UTxO.find (isScriptTxOut Head.validatorScript) utxoOfThisHead' ?> CannotFindHeadOutputInDecrement
  let balance = txOutValue headOut <> negateValue decommitValue
  when (isNegative balance) $
    Left DecrementValueNegative
  Right $ decrementTx scriptRegistry ownVerificationKey headId headParameters headUTxO sn sigs
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
    UTxO.findBy
      ( \(TxIn txid _, txout) ->
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
  Either NotAnInitReason (OnChainTx Tx, InitialState)
observeInit _ctx _allVerificationKeys tx = do
  observation <- observeInitTx tx
  headOut <- head <$> nonEmpty (txOuts' tx) ?> NoHeadOutput
  let initialThreadUTxO = (mkTxIn tx 0, toCtxUTxOTxOut headOut)
  pure (toEvent observation, toState initialThreadUTxO observation)
 where
  toEvent :: InitObservation -> OnChainTx Tx
  toEvent InitObservation{headParameters, headId, headSeed, participants} =
    OnInitTx{headId, headSeed, headParameters, participants}

  toState initialThreadUTxO InitObservation{headParameters, headId, headSeed} =
    InitialState
      { initialThreadOutput =
          InitialThreadOutput
            { initialThreadUTxO
            , initialParties = partyToChain <$> headParameters.parties
            , initialContestationPeriod = toChain headParameters.contestationPeriod
            }
      , initialInitials = initials
      , initialCommits = mempty
      , headId
      , seedTxIn = fromJust $ headSeedToTxIn headSeed
      }

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    map
      (bimap (mkTxIn tx) toCtxUTxOTxOut)
      initialOutputs

  isInitial :: TxOut era -> Bool
  isInitial = isScriptTxOut initialValidatorScript

-- ** InitialState transitions

-- | Observe an commit transition using a 'InitialState' and 'observeCommitTx'.
-- NOTE: This function is a bit fragile as it assumes commit output on first
-- output while the underlying observeCommitTx could deal with commit outputs
-- at any index. Only use this function in tests and benchmarks.
observeCommit ::
  ChainContext ->
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeCommit ctx st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCommitTx networkId utxo tx
  let CommitObservation{party, committed, headId = commitHeadId} = observation
  guard $ commitHeadId == headId
  let event = OnCommitTx{headId, party, committed}
  let st' =
        st
          { initialInitials =
              -- NOTE: A commit tx has been observed and thus we can
              -- remove all it's inputs from our tracked initials
              filter ((`notElem` txIns' tx) . fst) initialInitials
          , initialCommits =
              (mkTxIn tx 0, toCtxUTxOTxOut $ List.head (txOuts' tx)) : initialCommits
          }
  pure (event, st')
 where
  ChainContext{networkId} = ctx

  InitialState
    { initialCommits
    , initialInitials
    , headId
    } = st

-- | Observe an collect transition using a 'InitialState' and 'observeCollectComTx'.
-- This function checks the head id and ignores if not relevant.
observeCollect ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, OpenState)
observeCollect st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCollectComTx utxo tx
  let CollectComObservation{headId = collectComHeadId, utxoHash} = observation
  guard (headId == collectComHeadId)
  let event = OnCollectComTx{headId}
  let st' =
        OpenState
          { openUTxO = adjustUTxO tx utxo
          , headId
          , seedTxIn
          , openUtxoHash = utxoHash
          }
  pure (event, st')
 where
  InitialState
    { headId
    , seedTxIn
    } = st

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

-- | Maximum number of parties used in the generators.
maxGenParties :: Int
maxGenParties = 3

-- | Generate a 'ChainContext' and 'ChainState' within the known limits above, along with a
-- transaction that results in a transition away from it.
genChainStateWithTx :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
genChainStateWithTx =
  oneof
    [ genInitWithState
    , genAbortWithState
    , genCommitWithState
    , genIncrementWithState
    , genDecrementWithState
    , genCollectWithState
    , genCloseWithState
    , genContestWithState
    , genFanoutWithState
    ]
 where
  genInitWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genInitWithState = do
    ctx <- genHydraContext maxGenParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    let tx = initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
    pure (cctx, Idle, mempty, tx, Init)

  genAbortWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genAbortWithState = do
    ctx <- genHydraContext maxGenParties
    (cctx, stInitial) <- genStInitial ctx
    -- TODO: also generate sometimes aborts with utxo
    let utxo = getKnownUTxO stInitial
        InitialState{seedTxIn} = stInitial
        tx = unsafeAbort cctx seedTxIn utxo mempty
    pure (cctx, Initial stInitial, mempty, tx, Abort)

  genCommitWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genCommitWithState = do
    ctx <- genHydraContext maxGenParties
    (cctx, stInitial) <- genStInitial ctx
    utxo <- genCommit
    let InitialState{headId} = stInitial
    let tx = unsafeCommit cctx headId (getKnownUTxO stInitial) utxo
    pure (cctx, Initial stInitial, mempty, tx, Commit)

  genCollectWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genCollectWithState = do
    (ctx, _, st, utxo, tx) <- genCollectComTx
    pure (ctx, Initial st, utxo, tx, Collect)

  genIncrementWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genIncrementWithState = do
    (ctx, st, utxo, tx) <- genIncrementTx maxGenParties
    pure (ctx, Open st, utxo, tx, Increment)

  genDecrementWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genDecrementWithState = do
    (ctx, _, st, utxo, tx) <- genDecrementTx maxGenParties
    pure (ctx, Open st, utxo, tx, Decrement)

  genCloseWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genCloseWithState = do
    (ctx, st, utxo, tx, _) <- genCloseTx maxGenParties
    pure (ctx, Open st, utxo, tx, Close)

  genContestWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genContestWithState = do
    (hctx, _, st, utxo, tx) <- genContestTx
    ctx <- pickChainContext hctx
    pure (ctx, Closed st, utxo, tx, Contest)

  genFanoutWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genFanoutWithState = do
    (ctx, st, utxo, tx) <- genFanoutTx maxGenParties
    pure (ctx, Closed st, utxo, tx, Fanout)

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

-- | Generate a `HydraContext` for a arbitrary number of parties, bounded by
-- given maximum.
genHydraContext :: Int -> Gen HydraContext
genHydraContext maxParties = choose (1, maxParties) >>= genHydraContextFor

-- | Generate a 'HydraContext' for a given number of parties.
genHydraContextFor :: Int -> Gen HydraContext
genHydraContextFor n = do
  ctxVerificationKeys <- replicateM n genVerificationKey
  ctxHydraSigningKeys <- vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  ctxScriptRegistry <- genScriptRegistry
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxHydraSigningKeys
      , ctxNetworkId
      , ctxContestationPeriod
      , ctxScriptRegistry
      }

instance Arbitrary HydraContext where
  arbitrary = genHydraContext maxGenParties

-- | Get all peer-specific 'ChainContext's from a 'HydraContext'. NOTE: This
-- assumes that 'HydraContext' has same length 'ctxVerificationKeys' and
-- 'ctxHydraSigningKeys'.
-- XXX: This is actually a non-monadic function.
deriveChainContexts :: HydraContext -> Gen [ChainContext]
deriveChainContexts ctx = do
  pure $
    flip map (zip ctxVerificationKeys allParties') $ \(vk, p) ->
      ChainContext
        { networkId = ctxNetworkId
        , ownVerificationKey = vk
        , ownParty = p
        , scriptRegistry = ctxScriptRegistry
        }
 where
  allParties' = ctxParties ctx

  HydraContext
    { ctxVerificationKeys
    , ctxNetworkId
    , ctxScriptRegistry
    } = ctx

-- | Pick one of the participants and derive the peer-specific 'ChainContext'
-- from a 'HydraContext'. NOTE: This assumes that 'HydraContext' has same length
-- 'ctxVerificationKeys' and 'ctxHydraSigningKeys'.
pickChainContext :: HydraContext -> Gen ChainContext
pickChainContext ctx =
  deriveChainContexts ctx >>= elements

genStInitial ::
  HydraContext ->
  Gen (ChainContext, InitialState)
genStInitial ctx = do
  seedInput <- genTxIn
  cctx <- pickChainContext ctx
  let txInit = initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
  let initState = unsafeObserveInit cctx (ctxVerificationKeys ctx) txInit
  pure (cctx, initState)

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx = do
  cctx <- pickChainContext ctx
  seedInput <- genTxIn
  pure $ initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)

genCommits ::
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits =
  genCommits' genCommit

genCommits' ::
  Gen UTxO ->
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits' genUTxO ctx txInit = do
  -- Prepare UTxO to commit. We need to scale down the quantities by number of
  -- committed UTxOs to ensure we are not as easily hitting overflows of the max
  -- bound (Word64) when collecting all the commits together later.
  commitUTxOs <- forM (ctxParties ctx) $ const genUTxO
  let scaledCommitUTxOs = scaleCommitUTxOs commitUTxOs

  allChainContexts <- deriveChainContexts ctx
  forM (zip allChainContexts scaledCommitUTxOs) $ \(cctx, toCommit) -> do
    let stInitial@InitialState{headId} = unsafeObserveInit cctx (ctxVerificationKeys ctx) txInit
    pure $ unsafeCommit cctx headId (getKnownUTxO stInitial) toCommit
 where
  scaleCommitUTxOs :: [UTxO] -> [UTxO]
  scaleCommitUTxOs commitUTxOs =
    let numberOfUTxOs = length commitUTxOs
     in map (UTxO.map (modifyTxOutValue (scaleQuantitiesDownBy numberOfUTxOs))) commitUTxOs

  scaleQuantitiesDownBy :: Int -> Value -> Value
  scaleQuantitiesDownBy x =
    -- XXX: Foldable Value instance would be nice here
    IsList.fromList
      . map (\(an, Quantity q) -> (an, Quantity $ q `div` fromIntegral x))
      . IsList.toList

genCommitFor :: VerificationKey PaymentKey -> Gen UTxO
genCommitFor vkey =
  frequency
    [ (1, pure mempty)
    , (10, genOneUTxOFor vkey)
    ]

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

genCollectComTx :: Gen (ChainContext, [UTxO], InitialState, UTxO, Tx)
genCollectComTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committedUTxO, stInitialized) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) txInit commits
  let InitialState{headId} = stInitialized
  let utxoToCollect = fold committedUTxO
  let spendableUTxO = getKnownUTxO stInitialized
  pure (cctx, committedUTxO, stInitialized, mempty, unsafeCollect cctx headId (ctxHeadParameters ctx) utxoToCollect spendableUTxO)

genDepositTx :: Int -> Gen (HydraContext, OpenState, UTxO, Tx)
genDepositTx numParties = do
  ctx <- genHydraContextFor numParties
  utxo <- genUTxOAdaOnlyOfSize 1 `suchThat` (not . UTxO.null)
  (_, st@OpenState{headId}) <- genStOpen ctx
  -- NOTE: Not too high so we can use chooseEnum (which goes through Int) here and in other generators
  slot <- chooseEnum (0, 1_000_000)
  slotsUntilDeadline <- chooseEnum (0, 86400)
  let deadline = slotNoToUTCTime systemStart slotLength (slot + slotsUntilDeadline)
  let tx = depositTx (ctxNetworkId ctx) headId (mkSimpleBlueprintTx utxo) slot deadline
  pure (ctx, st, utxo <> utxoFromTx tx, tx)

genRecoverTx ::
  Gen (UTxO, Tx)
genRecoverTx = do
  (_, _, depositedUTxO, txDeposit) <- genDepositTx maximumNumberOfParties
  let DepositObservation{deposited, deadline} = fromJust $ observeDepositTx testNetworkId txDeposit
  let deadlineSlot = slotNoFromUTCTime systemStart slotLength deadline
  slotAfterDeadline <- chooseEnum (deadlineSlot, deadlineSlot + 86400)
  let tx = recoverTx (getTxId $ getTxBody txDeposit) deposited slotAfterDeadline
  pure (depositedUTxO, tx)

genIncrementTx :: Int -> Gen (ChainContext, OpenState, UTxO, Tx)
genIncrementTx numParties = do
  (ctx, st@OpenState{headId}, utxo, txDeposit) <- genDepositTx numParties
  cctx <- pickChainContext ctx
  let DepositObservation{deposited, depositTxId, deadline} = fromJust $ observeDepositTx (ctxNetworkId ctx) txDeposit
  let openUTxO = getKnownUTxO st
  let version = 0
  snapshot <- genConfirmedSnapshot headId version 1 openUTxO (Just deposited) Nothing (ctxHydraSigningKeys ctx)
  let deadlineSlot = slotNoFromUTCTime systemStart slotLength deadline
  slotBeforeDeadline <- chooseEnum (0, deadlineSlot)
  pure
    ( cctx
    , st
    , utxo
    , unsafeIncrement cctx (openUTxO <> utxo) headId (ctxHeadParameters ctx) snapshot depositTxId slotBeforeDeadline
    )

genDecrementTx :: Int -> Gen (ChainContext, UTxO, OpenState, UTxO, Tx)
genDecrementTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx `suchThat` \(u, _) -> not (UTxO.null u)
  cctx <- pickChainContext ctx
  let (confirmedUtxo, toDecommit) = splitUTxO u0
  let version = 0
  snapshot <- genConfirmedSnapshot headId version 1 confirmedUtxo Nothing (Just toDecommit) (ctxHydraSigningKeys ctx)
  let openUTxO = getKnownUTxO stOpen
  pure
    ( cctx
    , fromMaybe mempty (utxoToDecommit $ getSnapshot snapshot)
    , stOpen
    , mempty
    , unsafeDecrement cctx openUTxO headId (ctxHeadParameters ctx) snapshot
    )

genCloseTx :: Int -> Gen (ChainContext, OpenState, UTxO, Tx, ConfirmedSnapshot Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  let (inHead, toDecommit) = splitUTxO u0
  n <- elements [1 .. 10]
  utxoToCommit' <- oneof [Just <$> genUTxOAdaOnlyOfSize n, pure Nothing]
  utxoToDecommit' <- oneof [pure toDecommit, pure mempty]
  let (confirmedUTxO, utxoToCommit, utxoToDecommit) =
        if isNothing utxoToCommit'
          then (inHead, Nothing, if utxoToDecommit' == mempty then Nothing else Just utxoToDecommit')
          else (u0, utxoToCommit', Nothing)
  let version = 0
  snapshot <- genConfirmedSnapshot headId version 1 confirmedUTxO utxoToCommit utxoToDecommit (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let utxo = getKnownUTxO stOpen
  pure (cctx, stOpen, utxo, unsafeClose cctx utxo headId (ctxHeadParameters ctx) version snapshot startSlot pointInTime, snapshot)

genContestTx :: Gen (HydraContext, PointInTime, ClosedState, UTxO, Tx)
genContestTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  let (confirmedUTxO, utxoToDecommit) = splitUTxO u0
  let version = 1
  confirmed <- genConfirmedSnapshot headId version 1 confirmedUTxO Nothing (Just utxoToDecommit) []
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let openUTxO = getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) version confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  let utxo = getKnownUTxO stClosed
  someUtxo <- genUTxO1 genTxOut
  let (confirmedUTxO', utxoToDecommit') = splitUTxO someUtxo
  contestSnapshot <- genConfirmedSnapshot headId version (succ $ number $ getSnapshot confirmed) confirmedUTxO' Nothing (Just utxoToDecommit') (ctxHydraSigningKeys ctx)
  contestPointInTime <- genPointInTimeBefore stClosed.contestationDeadline
  pure (ctx, closePointInTime, stClosed, mempty, unsafeContest cctx utxo headId cp version contestSnapshot contestPointInTime)

genFanoutTx :: Int -> Gen (ChainContext, ClosedState, UTxO, Tx)
genFanoutTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  n <- elements [1 .. 10]
  toCommit' <- Just <$> genUTxOAdaOnlyOfSize n
  openVersion <- elements [0, 1]
  version <- elements [0, 1]
  confirmed <- genConfirmedSnapshot headId version 1 u0 toCommit' Nothing (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let openUTxO = getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) openVersion confirmed startSlot closePointInTime
  let stClosed@ClosedState{seedTxIn} = snd $ fromJust $ observeClose stOpen txClose
  let toFanout = utxo $ getSnapshot confirmed
  let toCommit = utxoToCommit $ getSnapshot confirmed
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
  let spendableUTxO = getKnownUTxO stClosed
  -- if local version is not matching the snapshot version we **should** fanout commit utxo
  let finalToCommit = if openVersion /= version then toCommit else Nothing
  pure (cctx, stClosed, mempty, unsafeFanout cctx spendableUTxO seedTxIn toFanout finalToCommit Nothing deadlineSlotNo)

genStOpen ::
  HydraContext ->
  Gen (UTxO, OpenState)
genStOpen ctx = do
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) txInit commits
  let InitialState{headId} = stInitial
  let utxoToCollect = fold committed
  let spendableUTxO = getKnownUTxO stInitial
  let txCollect = unsafeCollect cctx headId (ctxHeadParameters ctx) utxoToCollect spendableUTxO
  pure (utxoToCollect, snd . fromJust $ observeCollect stInitial txCollect)

genStClosed ::
  HydraContext ->
  UTxO ->
  Maybe UTxO ->
  Maybe UTxO ->
  Gen (SnapshotNumber, UTxO, Maybe UTxO, Maybe UTxO, ClosedState)
genStClosed ctx utxo utxoToCommit utxoToDecommit = do
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  confirmed <- arbitrary
  let (sn, snapshot, toFanout, toCommit, toDecommit, v) = case confirmed of
        InitialSnapshot{} ->
          ( 0
          , InitialSnapshot{headId, initialUTxO = u0}
          , u0
          , Nothing
          , Nothing
          , 0
          )
        ConfirmedSnapshot{snapshot = snap, signatures} ->
          ( number snap
          , ConfirmedSnapshot
              { snapshot = snap{utxo = utxo, utxoToDecommit, utxoToCommit}
              , signatures
              }
          , utxo
          , utxoToCommit
          , utxoToDecommit
          , version snap
          )
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let utxo' = getKnownUTxO stOpen
  let txClose = unsafeClose cctx utxo' headId (ctxHeadParameters ctx) v snapshot startSlot pointInTime
  pure (sn, toFanout, toCommit, toDecommit, snd . fromJust $ observeClose stOpen txClose)

-- ** Danger zone

unsafeCommit ::
  HasCallStack =>
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  -- | 'UTxO' to commit. All outputs are assumed to be owned by public keys.
  UTxO ->
  Tx
unsafeCommit ctx headId spendableUTxO utxoToCommit =
  either (error . show) id $ commit ctx headId spendableUTxO utxoToCommit

unsafeAbort ::
  HasCallStack =>
  ChainContext ->
  -- | Seed TxIn
  TxIn ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Committed UTxOs to reimburse.
  UTxO ->
  Tx
unsafeAbort ctx seedTxIn spendableUTxO committedUTxO =
  either (error . show) id $ abort ctx seedTxIn spendableUTxO committedUTxO

unsafeIncrement ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable 'UTxO'
  UTxO ->
  HeadId ->
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
  HeadId ->
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

unsafeCollect ::
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  Tx
unsafeCollect ctx headId headParameters utxoToCollect spendableUTxO =
  either (error . show) id $ collect ctx headId headParameters utxoToCollect spendableUTxO

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
  InitialState
unsafeObserveInit cctx txInit allVerificationKeys =
  case observeInit cctx txInit allVerificationKeys of
    Left err -> error $ "Did not observe an init tx: " <> show err
    Right st -> snd st

-- REVIEW: Maybe it would be more convenient if 'unsafeObserveInitAndCommits'
-- returns just 'UTXO' instead of [UTxO]
unsafeObserveInitAndCommits ::
  HasCallStack =>
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  [Tx] ->
  ([UTxO], InitialState)
unsafeObserveInitAndCommits ctx allVerificationKeys txInit commits =
  (utxo, stInitial')
 where
  stInitial = unsafeObserveInit ctx allVerificationKeys txInit

  (utxo, stInitial') = flip runState stInitial $ do
    forM commits $ \txCommit -> do
      st <- get
      let (event, st') = fromJust $ observeCommit ctx st txCommit
      put st'
      pure $ case event of
        OnCommitTx{committed} -> committed
        _ -> mempty
