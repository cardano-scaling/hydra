{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  ExecutionUnits (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  SlotNo,
  Tx,
  TxIn,
  UTxO,
  getTxBody,
  getTxId,
  modifyTxOutValue,
  negateValue,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainState (..),
  ChainStateAt (..),
  ClosedState (..),
  HasKnownUTxO (..),
  HydraContext (..),
  OpenState (..),
  ctxHeadParameters,
  ctxParticipants,
  ctxParties,
  initialize,
  observeClose,
  unsafeClose,
  unsafeContest,
  unsafeDecrement,
  unsafeFanout,
  unsafeFinalPartialFanout,
  unsafeIncrement,
  unsafeObserveInit,
  unsafePartialFanout,
 )
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  getSnapshot,
  mkSimpleBlueprintTx,
  txInToHeadSeed,
  utxoFromTx,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.Deposit (DepositObservation (..), depositTx, observeDepositTx)
import Hydra.Tx.Increment (IncrementObservation (..), observeIncrementTx)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Utils (splitUTxO)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, evaluateTx', maxCpu, maxMem, slotLength, systemStart)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId)
import Test.Hydra.Tx.Gen (
  genConfirmedSnapshot,
  genPointInTimeBefore,
  genScriptRegistry,
  genTxOut,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genUTxOSized,
  genValidityBoundsFromContestationPeriod,
  genVerificationKey,
 )
import Test.QuickCheck (choose, chooseEnum, elements, oneof, suchThat, vector)

instance Arbitrary ChainStateAt where
  arbitrary = genericArbitrary
  shrink = genericShrink

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

instance Arbitrary OpenState where
  arbitrary = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStOpen ctx

  shrink = genericShrink

-- | A definition of all transitions between 'ChainState's. Enumerable and
-- bounded to be used as labels for checking coverage.
data ChainTransition
  = Init
  | Deposit
  | Recover
  | Increment
  | Decrement
  | Close
  | Contest
  | Fanout
  | PartialFanout
  | FinalPartialFanout
  deriving stock (Eq, Show, Enum, Bounded)

-- | Generate a 'ChainContext' and 'ChainState' within the known limits above, along with a
-- transaction that results in a transition away from it.
genChainStateWithTx :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
genChainStateWithTx =
  oneof
    [ genInitWithState
    , genDepositWithState
    , genRecoverWithState
    , genIncrementWithState
    , genDecrementWithState
    , genCloseWithState
    , genContestWithState
    , genFanoutWithState
    , genPartialFanoutWithState
    , genFinalPartialFanoutWithState
    ]
 where
  genInitWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genInitWithState = do
    ctx <- genHydraContext maxGenParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    let tx = initialize cctx defaultPParams seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
    pure (cctx, Idle, mempty, tx, Init)

  genDepositWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genDepositWithState = do
    (ctx, st, utxoToDeposit, tx) <- genDepositTx maxGenParties
    cctx <- pickChainContext ctx
    pure (cctx, Open st, utxoToDeposit, tx, Deposit)

  genRecoverWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genRecoverWithState = do
    -- Inlined from genRecoverTx so we can keep the OpenState whose headId
    -- matches the recovered deposit; tests that join on headId need them
    -- to agree.
    (ctx, st, _, txDeposit) <- genDepositTx maxGenParties
    let DepositObservation{deposited, deadline} =
          fromJust $ observeDepositTx (ctxNetworkId ctx) txDeposit
    let deadlineSlot = slotNoFromUTCTime systemStart slotLength deadline
    slotAfterDeadline <- chooseEnum (deadlineSlot, deadlineSlot + 86400)
    let tx = recoverTx (getTxId $ getTxBody txDeposit) deposited slotAfterDeadline
    cctx <- pickChainContext ctx
    pure (cctx, Open st, utxoFromTx txDeposit, tx, Recover)

  genIncrementWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genIncrementWithState = do
    (ctx, st, utxo, tx) <- genIncrementTx maxGenParties
    cctx <- pickChainContext ctx
    pure (cctx, Open st, utxo, tx, Increment)

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

  genPartialFanoutWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genPartialFanoutWithState = do
    (ctx, st, _, tx) <- genPartialFanoutTx maxGenParties
    pure (ctx, Closed st, mempty, tx, PartialFanout)

  genFinalPartialFanoutWithState :: Gen (ChainContext, ChainState, UTxO, Tx, ChainTransition)
  genFinalPartialFanoutWithState = do
    (ctx, st, fanoutProgressUTxO, tx) <- genFinalPartialFanoutTx maxGenParties
    pure (ctx, Closed st, fanoutProgressUTxO, tx, FinalPartialFanout)

-- \** Warning zone

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

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx = do
  cctx <- pickChainContext ctx
  seedInput <- genTxIn
  pure $ initialize cctx defaultPParams seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)

genDepositTx :: Int -> Gen (HydraContext, OpenState, UTxO, Tx)
genDepositTx numParties = do
  ctx <- genHydraContextFor numParties
  utxoToDeposit <- genUTxOAdaOnlyOfSize 1 `suchThat` (not . UTxO.null)
  (_, st@OpenState{headId}) <- genStOpen ctx
  -- NOTE: Not too high so we can use chooseEnum (which goes through Int) here and in other generators
  slot <- chooseEnum (0, 1_000_000)
  slotsUntilDeadline <- chooseEnum (0, 86400)
  let deadline = slotNoToUTCTime systemStart slotLength (slot + slotsUntilDeadline)
  let tx = depositTx (ctxNetworkId ctx) defaultPParams headId (mkSimpleBlueprintTx utxoToDeposit) slot deadline Nothing
  pure (ctx, st, utxoToDeposit, tx)

genRecoverTx ::
  Gen (UTxO, Tx)
genRecoverTx = do
  (_, _, _, txDeposit) <- genDepositTx maximumNumberOfParties
  let DepositObservation{deposited, deadline} = fromJust $ observeDepositTx testNetworkId txDeposit
  let deadlineSlot = slotNoFromUTCTime systemStart slotLength deadline
  slotAfterDeadline <- chooseEnum (deadlineSlot, deadlineSlot + 86400)
  let tx = recoverTx (getTxId $ getTxBody txDeposit) deposited slotAfterDeadline
  pure (utxoFromTx txDeposit, tx)

genIncrementTx ::
  Int ->
  Gen
    ( HydraContext
    , OpenState
    , UTxO -- Deposited / to be incremented
    , Tx
    )
genIncrementTx numParties = do
  (ctx, st@OpenState{seedTxIn, headId}, _, txDeposit) <- genDepositTx numParties
  cctx <- pickChainContext ctx
  let DepositObservation{deposited, depositTxId, deadline} = fromJust $ observeDepositTx (ctxNetworkId ctx) txDeposit
  let openUTxO = getKnownUTxO st
  let version = 0
  -- XXX: openUTxO can't be right here
  snapshot <- genConfirmedSnapshot headId version 1 openUTxO (Just deposited) Nothing (ctxHydraSigningKeys ctx)
  let deadlineSlot = slotNoFromUTCTime systemStart slotLength deadline
  slotBeforeDeadline <- chooseEnum (0, deadlineSlot)
  pure
    ( ctx
    , st
    , openUTxO <> utxoFromTx txDeposit
    , unsafeIncrement
        cctx
        (openUTxO <> utxoFromTx txDeposit)
        (txInToHeadSeed seedTxIn, headId)
        (ctxHeadParameters ctx)
        snapshot
        depositTxId
        slotBeforeDeadline
    )

genDecrementTx :: Int -> Gen (ChainContext, UTxO, OpenState, UTxO, Tx)
genDecrementTx numParties = do
  (ctx, stOpen@OpenState{seedTxIn, headId}, utxo, txIncrement) <- genIncrementTx numParties
  let IncrementObservation{newVersion, deposited} = fromJust $ observeIncrementTx (ctxNetworkId ctx) utxo txIncrement
  let (confirmedUtxo, toDecommit) = splitUTxO deposited
  cctx <- pickChainContext ctx
  snapshot <- genConfirmedSnapshot headId newVersion 1 confirmedUtxo Nothing (Just toDecommit) (ctxHydraSigningKeys ctx)
  let utxoSpendable = utxoFromTx txIncrement
  pure
    ( cctx
    , fromMaybe mempty (utxoToDecommit $ getSnapshot snapshot)
    , stOpen
    , utxoSpendable
    , unsafeDecrement
        cctx
        utxoSpendable
        (txInToHeadSeed seedTxIn, headId)
        (ctxHeadParameters ctx)
        snapshot
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
  -- genStOpen inflated the head by u0Value; adjust so the head holds exactly
  -- headAdaOverhead + confirmedUTxOValue + decommitValue at close time.
  let u0Value = UTxO.totalValue u0
      confirmedUTxOValue = UTxO.totalValue confirmedUTxO
      decommitValue = maybe mempty UTxO.totalValue utxoToDecommit
  let utxo = UTxO.map (modifyTxOutValue (<> confirmedUTxOValue <> decommitValue <> negateValue u0Value)) $ getKnownUTxO stOpen
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
  openVersion <- elements [0, 1]
  version <- elements [0, 1]
  -- Only generate commit UTxO when version differs so the accumulator commitment
  -- in the closed datum matches what fanoutTx builds. Size bounded for KZG budget
  -- with maximumNumberOfParties: u0 ≤ 5, commit ≤ 5, total ≤ 10 outputs.
  n <- elements [1 .. 5]
  toCommit' <-
    if openVersion /= version
      then Just <$> genUTxOAdaOnlyOfSize n
      else pure Nothing
  confirmed <- genConfirmedSnapshot headId version 1 u0 toCommit' Nothing (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  -- When increment was applied (openVersion /= version), the head already holds the
  -- committed UTxOs' value. Enrich the open UTxO before close so headAdaOverhead is
  -- computed correctly as non-negative.
  let commitEnrichment = if openVersion /= version then maybe mempty UTxO.totalValue toCommit' else mempty
  let openUTxO = UTxO.map (modifyTxOutValue (<> commitEnrichment)) $ getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) openVersion confirmed startSlot closePointInTime
  let stClosed@ClosedState{seedTxIn} = snd $ fromJust $ observeClose stOpen txClose
  let toFanout = utxo $ getSnapshot confirmed
  let toCommit = utxoToCommit $ getSnapshot confirmed
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
  -- if local version is not matching the snapshot version we **should** fanout commit utxo
  let finalToCommit = if openVersion /= version then toCommit else Nothing
  let spendableUTxO = getKnownUTxO stClosed
  let utxoForProof = toFanout <> fold toCommit
  pure (cctx, stClosed, spendableUTxO, unsafeFanout cctx spendableUTxO seedTxIn toFanout finalToCommit Nothing utxoForProof deadlineSlotNo)

genPartialFanoutTx :: Int -> Gen (ChainContext, ClosedState, UTxO, Tx)
genPartialFanoutTx numParties = do
  (cctx, stClosed@ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0) <-
    genClosedStateForFanout numParties
  let evalUTxO = spendableUTxO <> getKnownUTxO cctx
      (_, tx) = findFittingPartialChunk evalUTxO cctx spendableUTxO seedTxIn u0 deadlineSlotNo
  pure (cctx, stClosed, spendableUTxO, tx)

-- | Generate a final partial fanout transaction that chains from a partial fanout step.
-- Returns the FanoutProgress UTxO (output of the intermediate step) as the 3rd element,
-- since it is the spendable UTxO required for transaction evaluation.
genFinalPartialFanoutTx :: Int -> Gen (ChainContext, ClosedState, UTxO, Tx)
genFinalPartialFanoutTx numParties = do
  (cctx, stClosed@ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0) <-
    genClosedStateForFanout numParties
  let evalUTxO = spendableUTxO <> getKnownUTxO cctx
      (k, partialTx) = findFittingPartialChunk evalUTxO cctx spendableUTxO seedTxIn u0 deadlineSlotNo
      remainingUTxO = UTxO.fromList (drop k (UTxO.toList u0))
      fanoutProgressUTxO = utxoFromTx partialTx
  pure (cctx, stClosed, fanoutProgressUTxO, unsafeFinalPartialFanout cctx fanoutProgressUTxO seedTxIn remainingUTxO deadlineSlotNo)

-- | Shared setup for partial and final-partial fanout generators.
-- Produces a closed head with some UTxOs in the snapshot (at least 2 entries so
-- the first step distributes at least 1 and leaves at least 1).
genClosedStateForFanout ::
  Int ->
  Gen (ChainContext, ClosedState, UTxO, SlotNo, UTxO)
genClosedStateForFanout numParties = do
  ctx <- genHydraContextFor numParties
  n <- choose (11, 18 :: Int)
  u0 <- genUTxOAdaOnlyOfSize n
  (_, stOpen@OpenState{headId}) <- genStOpen ctx
  let version = 0
  confirmed <- genConfirmedSnapshot headId version 1 u0 Nothing Nothing (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  -- Enrich the open head output with u0Value BEFORE building the close tx so that
  -- headAdaOverhead (= headLovelace - snapshotLovelace) is non-negative and correct.
  -- genStOpen already inflated the head with a small u0; we add the snapshot u0 here.
  let u0Value = UTxO.totalValue u0
  let openUTxO = UTxO.map (modifyTxOutValue (<> u0Value)) $ getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) version confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
  pure (cctx, stClosed, getKnownUTxO stClosed, deadlineSlotNo, u0)

-- | Generate a closed state where a decommit was applied on-chain before close,
-- i.e. @ClosedState.version > snapshot.version@. The closed datum accumulator
-- includes the decommit UTxOs; the distribution set (u0) excludes them.
genClosedStateWithAppliedDecommit ::
  Int ->
  Gen (ChainContext, ClosedState, UTxO, SlotNo, UTxO, UTxO)
genClosedStateWithAppliedDecommit numParties = do
  ctx <- genHydraContextFor numParties
  n <- choose (11, 18 :: Int)
  u0 <- genUTxOAdaOnlyOfSize n
  nDecommit <- choose (1, 3 :: Int)
  decommitUTxO <- genUTxOAdaOnlyOfSize nDecommit
  (_, stOpen@OpenState{headId}) <- genStOpen ctx
  let snapshotVersion = 0
      openVersion = 1
  confirmed <- genConfirmedSnapshot headId snapshotVersion 1 u0 Nothing (Just decommitUTxO) (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let u0Value = UTxO.totalValue u0
  let openUTxO = UTxO.map (modifyTxOutValue (<> u0Value)) $ getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) openVersion confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
  pure (cctx, stClosed, getKnownUTxO stClosed, deadlineSlotNo, u0, decommitUTxO)

-- | Find the largest chunk size for a partial fanout tx that evaluates within the
-- full execution budget. Returns the chunk size used and the built transaction.
findFittingPartialChunk :: UTxO -> ChainContext -> UTxO -> TxIn -> UTxO -> SlotNo -> (Int, Tx)
findFittingPartialChunk evalUTxO cctx spendableUTxO seedTxIn u0 deadlineSlotNo =
  go [UTxO.size u0 - 1, UTxO.size u0 - 2 .. 1]
 where
  go [] = error "findFittingPartialChunk: no fitting chunk size found"
  go (n : rest) =
    let tx = unsafePartialFanout cctx spendableUTxO seedTxIn n u0 deadlineSlotNo
     in case evaluateTx tx evalUTxO of
          Right report | all isRight (Map.elems report) -> (n, tx)
          _ -> go rest

-- | Like 'genClosedStateForFanout' but uses complex UTxO (multi-asset tokens via 'genUTxOSized').
genClosedStateForFanoutWithComplexUTxO ::
  Int ->
  Gen (ChainContext, ClosedState, UTxO, SlotNo, UTxO)
genClosedStateForFanoutWithComplexUTxO numParties = do
  ctx <- genHydraContextFor numParties
  let n = 11
  u0 <- genUTxOSized n
  (_, stOpen@OpenState{headId}) <- genStOpen ctx
  let version = 0
  confirmed <- genConfirmedSnapshot headId version 1 u0 Nothing Nothing (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let u0Value = UTxO.totalValue u0
  let openUTxO = UTxO.map (modifyTxOutValue (<> u0Value)) $ getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) version confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength stClosed.contestationDeadline
  pure (cctx, stClosed, getKnownUTxO stClosed, deadlineSlotNo, u0)

-- | Like 'genPartialFanoutTx' but uses complex UTxO with multi-asset tokens.
-- Applies the dynamic chunk-size algorithm using a 90% safety budget, so the
-- returned transaction is guaranteed to evaluate within that budget.
genPartialFanoutTxWithComplexUTxO :: Int -> Gen (ChainContext, ClosedState, UTxO, Tx)
genPartialFanoutTxWithComplexUTxO numParties = do
  (cctx, stClosed@ClosedState{seedTxIn}, spendableUTxO, deadlineSlotNo, u0) <-
    genClosedStateForFanoutWithComplexUTxO numParties
  let evalUTxO = spendableUTxO <> getKnownUTxO cctx
      safeUnits =
        ExecutionUnits
          { executionMemory = maxMem * 9 `div` 10
          , executionSteps = maxCpu * 9 `div` 10
          }
  pure
    ( cctx
    , stClosed
    , spendableUTxO
    , findFittingChunk safeUnits evalUTxO cctx spendableUTxO seedTxIn u0 deadlineSlotNo
    )
 where
  findFittingChunk safeUnits evalUTxO cctx spendableUTxO seedTxIn u0 deadlineSlotNo =
    go [UTxO.size u0 - 1, UTxO.size u0 - 2 .. 1]
   where
    go [] = error "genPartialFanoutTxWithComplexUTxO: no fitting chunk size found"
    go (n : rest) =
      let tx = unsafePartialFanout cctx spendableUTxO seedTxIn n u0 deadlineSlotNo
       in case evaluateTx' safeUnits tx evalUTxO of
            Right report | all isRight (Map.elems report) -> tx
            _ -> go rest

genStOpen ::
  HydraContext ->
  Gen (UTxO, OpenState)
genStOpen ctx = do
  txInit <- genInitTx ctx
  cctx <- pickChainContext ctx
  let stOpen = unsafeObserveInit cctx (ctxVerificationKeys ctx) txInit
  n <- elements [1 .. 5]
  u0 <- genUTxOAdaOnlyOfSize n
  -- Init head output carries only tokens (0 ADA); inflate it so fanout can cover u0 outputs.
  let u0Value = UTxO.totalValue u0
  let stOpen' = stOpen{openUTxO = UTxO.map (modifyTxOutValue (<> u0Value)) (openUTxO stOpen)}
  pure (u0, stOpen')

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
          , InitialSnapshot{headId}
          , u0
          , Nothing
          , Nothing
          , 0
          )
        ConfirmedSnapshot{snapshot = snap, signatures} ->
          let accumulator = Accumulator.buildFromSnapshotUTxOs utxo utxoToCommit utxoToDecommit
           in ( number snap
              , ConfirmedSnapshot
                  { snapshot = snap{utxo = utxo, utxoToDecommit, utxoToCommit, accumulator}
                  , signatures
                  }
              , utxo
              , utxoToCommit
              , utxoToDecommit
              , Hydra.Tx.version snap
              )
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let utxo' = getKnownUTxO stOpen
  let txClose = unsafeClose cctx utxo' headId (ctxHeadParameters ctx) v snapshot startSlot pointInTime
  pure (sn, toFanout, toCommit, toDecommit, snd . fromJust $ observeClose stOpen txClose)

-- | Maximum number of parties used in the generators.
maxGenParties :: Int
maxGenParties = 3
