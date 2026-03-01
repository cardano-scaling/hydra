{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Chain.Direct.State where

import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Prelude hiding (init)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)

import GHC.IsList qualified as IsList

import Hydra.Cardano.Api (
  Key (VerificationKey),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  Quantity (..),
  Tx,
  UTxO,
  Value,
  getTxBody,
  getTxId,
  modifyTxOutValue,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct.State (ChainContext (..), ChainState (..), ChainStateAt (..), ChainTransition (..), ClosedState (..), HasKnownUTxO (..), HydraContext (..), InitialState (..), OpenState (..), ctxHeadParameters, ctxParticipants, ctxParties, initialize, observeClose, observeCollect, unsafeAbort, unsafeClose, unsafeCollect, unsafeCommit, unsafeContest, unsafeDecrement, unsafeFanout, unsafeIncrement, unsafeObserveInit, unsafeObserveInitAndCommits)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  getSnapshot,
  mkSimpleBlueprintTx,
  utxoFromTx,
 )
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.Deposit (DepositObservation (..), depositTx, observeDepositTx)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Utils (splitUTxO)
import Test.Hydra.Ledger.Cardano.Fixtures (slotLength, systemStart)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId)
import Test.Hydra.Tx.Gen (
  genConfirmedSnapshot,
  genOneUTxOFor,
  genPointInTimeBefore,
  genScriptRegistry,
  genTxOut,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genValidityBoundsFromContestationPeriod,
  genVerificationKey,
 )
import Test.QuickCheck (choose, chooseEnum, elements, frequency, oneof, suchThat, vector)

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
  let tx = depositTx (ctxNetworkId ctx) defaultPParams headId (mkSimpleBlueprintTx utxo) slot deadline Nothing
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
