{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Transaction cost benchmarking using only hydra-tx primitives.
--
-- This module generates Hydra protocol transactions and measures their costs
-- (size, memory, CPU) without depending on hydra-node's chain state management.
--
-- All 9 Hydra protocol transactions are implemented:
-- * Init - direct call to initTx
-- * Commit - uses initTx + observeInitTx + commitTx
-- * CollectCom - uses init + collectComTx
-- * Increment - uses transaction chain + snapshot with utxoToCommit
-- * Decrement - uses transaction chain + snapshot with utxoToDecommit
-- * Close - uses transaction chain (Init → CollectCom) + closeTx
-- * Contest - uses transaction chain (Init → CollectCom → Close) + contestTx
-- * Abort - simplified implementation with abortTx
-- * FanOut - uses transaction chain (Init → CollectCom → Close) + fanoutTx
--
-- Approach:
-- Transaction chains are built by chaining observations:
-- 1. Generate Init tx, observe to get HeadId
-- 2. Generate CollectCom tx, extract OpenThreadOutput
-- 3. Generate Close tx, extract ClosedThreadOutput
-- 4. Use ThreadOutputs and snapshots for subsequent transactions
--
-- No hydra-node dependencies - uses only hydra-tx transaction primitives.
module TxCost where

import Hydra.Prelude hiding (catch)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hydra.Cardano.Api (
  Coin (..),
  CtxUTxO,
  ExecutionUnits (..),
  NetworkId (..),
  NetworkMagic (..),
  PaymentKey,
  SlotNo,
  Tx,
  TxBody,
  TxId,
  TxIn (..),
  TxIx (..),
  TxOut,
  UTxO,
  VerificationKey,
  getTxBody,
  getTxId,
  shelleyBasedEra,
  toCtxUTxOTxOut,
  txIns',
  txOuts',
  verificationKeyHash,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.Tx (txSpendingUTxO)
import Hydra.Cardano.Api.TxOut (toPlutusTxOut)
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Tx.Evaluate (
  estimateMinFee,
  evaluateTx,
  maxTxSize,
  slotLength,
  systemStart,
  usedExecutionUnits,
 )
import Hydra.Tx.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.Abort (abortTx, observeAbortTx)
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.Close (PointInTime, OpenThreadOutput (..), closeTx, observeCloseTx)
import Hydra.Tx.CollectCom (collectComTx, observeCollectComTx)
import Hydra.Tx.Commit (commitTx, observeCommitTx)
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain, toChain)
import Hydra.Tx.Crypto (HydraKey, SigningKey, MultiSignature, aggregate, sign)
import Hydra.Tx.Decrement (decrementTx, observeDecrementTx)
import Hydra.Tx.Fanout (fanoutTx)
import Hydra.Tx.HeadId (HeadId, mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (InitObservation (..), initTx, observeInitTx)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Hydra.Tx.Utils (IncrementalAction (..))
import PlutusLedgerApi.V3 (toBuiltinData)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins (lengthOfByteString, serialiseData)
import Test.Hydra.Tx.Gen (
  genConfirmedSnapshot,
  genOutputFor,
  genPointInTimeBefore,
  genScriptRegistry,
  genUTxOAdaOnlyOfSize,
  genValidityBoundsFromContestationPeriod,
  genVerificationKey,
 )
import Test.QuickCheck (oneof, vector)

-- * Cost computation functions

computeInitCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeInitCost = computeCostWithLimit 100 compute
 where
  compute numParties = do
    (tx, knownUtxo) <- genInitTx numParties
    evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx knownUtxo

  genInitTx :: Int -> Gen (Tx, UTxO)
  genInitTx numParties = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    seedInput <- genTxIn
    seedOutput <- genOutputFor =<< arbitrary
    participants <- replicateM numParties (arbitrary :: Gen OnChainId)
    headParameters <- genHeadParameters numParties
    let utxo = UTxO.singleton seedInput seedOutput
        tx = initTx networkId seedInput participants headParameters
    pure (tx, utxo)

computeCommitCost :: Gen [(NumUTxO, TxSize, MemUnit, CpuUnit, Coin)]
computeCommitCost = computeCostWithLimit 100 compute
 where
  compute numUTxO = do
    utxo <- genUTxOAdaOnlyOfSize numUTxO
    (commitTxResult, knownUtxo) <- genCommitTx utxo
    case commitTxResult of
      Nothing -> pure Nothing
      Just tx ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumUTxO $ UTxO.size utxo, txSize, memUnit, cpuUnit, minFee)) tx (utxo <> knownUtxo)

  genCommitTx :: UTxO -> Gen (Maybe Tx, UTxO)
  genCommitTx utxo = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    scriptRegistry <- genScriptRegistry

    -- Generate an init transaction first
    seedInput <- genTxIn
    participant <- arbitrary :: Gen OnChainId
    headParameters <- genHeadParameters 1
    let initTx' = initTx networkId seedInput [participant] headParameters

    -- Observe the init transaction
    case observeInitTx initTx' of
      Left _err -> pure (Nothing, mempty)
      Right observation -> do
        let headId = observation.headId
            -- Extract outputs from init tx and find the initial output (contains PT for participant)
            initialPairs = txOutputsToUTxOPairs initTx'
        case initialPairs of
          [] -> pure (Nothing, mempty)
          ((initialInput, initialOutput):_) -> do
            case parties headParameters of
              [] -> pure (Nothing, mempty)
              (party:_) -> do
                let vkh = verificationKeyHash (error "dummy vk for benchmarking" :: VerificationKey PaymentKey)
                    knownUTxO = UTxO.singleton initialInput initialOutput
                    blueprintTx = txSpendingUTxO utxo
                    commitBlueprintTx = CommitBlueprintTx utxo blueprintTx
                    tx = commitTx networkId scriptRegistry headId party commitBlueprintTx (initialInput, initialOutput, vkh)
                pure (Just tx, knownUTxO)

computeCollectComCost :: Gen [(NumParties, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 10]
 where
  compute numParties = do
    result <- genCollectComTx numParties
    case result of
      Nothing -> pure Nothing
      Just (utxo, tx, knownUtxo) ->
        case checkSizeAndEvaluate tx knownUtxo of
          Just (txSize, memUnit, cpuUnit, minFee) ->
            pure $ Just (NumParties numParties, serializedSize utxo, txSize, memUnit, cpuUnit, minFee)
          Nothing ->
            pure Nothing

  genCollectComTx :: Int -> Gen (Maybe (UTxO, Tx, UTxO))
  genCollectComTx numParties = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    scriptRegistry <- genScriptRegistry
    vk <- genVerificationKey

    -- Generate init transaction
    seedInput <- genTxIn
    participants <- replicateM numParties (arbitrary :: Gen OnChainId)
    headParameters <- genHeadParameters numParties
    let initTx' = initTx networkId seedInput participants headParameters

    case observeInitTx initTx' of
      Left _err -> pure Nothing
      Right observation -> do
        let headId = observation.headId
            initPairs = txOutputsToUTxOPairs initTx'

        -- Generate commit transactions for each party
        -- For benchmarking, we'll create simplified commits
        utxosToCommit <- replicateM numParties (genUTxOAdaOnlyOfSize 1)
        let utxoToCollect = fold utxosToCommit

        -- Extract head output and initial outputs from init tx
        case initPairs of
          [] -> pure Nothing
          (headPair : initialPairs) -> do
            let commits = Map.fromList initialPairs
                knownUTxO = UTxO.fromList (headPair : initialPairs)
                tx = collectComTx networkId scriptRegistry vk headId headParameters headPair commits utxoToCollect

            pure $ Just (utxoToCollect, tx, knownUTxO)

-- * Remaining transaction cost computations

computeCloseCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeCloseCost = computeCostWithLimit 50 compute
 where
  compute numParties = do
    result <- genCloseTx numParties
    case result of
      Nothing -> pure Nothing
      Just (tx, utxo) ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx utxo

  genCloseTx :: Int -> Gen (Maybe (Tx, UTxO))
  genCloseTx numParties = do
    -- Build chain: Init → CollectCom → Close
    chainResult <- genOpenHeadChain numParties
    case chainResult of
      Nothing -> pure Nothing
      Just (headId, openThreadOutput, utxo, parties, signingKeys) -> do
        -- Generate a confirmed snapshot
        snapshotUTxO <- genUTxOAdaOnlyOfSize 1
        snapshot <- genConfirmedSnapshot headId 0 1 snapshotUTxO Nothing mempty signingKeys

        -- Generate validity bounds
        scriptRegistry <- genScriptRegistry
        vk <- genVerificationKey
        contestationPeriod <- arbitrary :: Gen ContestationPeriod
        (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod contestationPeriod

        let tx = closeTx scriptRegistry vk headId 0 snapshot startSlot pointInTime openThreadOutput NoThing
            allUTxO = utxo <> UTxO.fromList [openThreadOutput.openThreadUTxO]

        pure $ Just (tx, allUTxO)

computeContestCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeContestCost = computeCostWithLimit 50 compute
 where
  compute numParties = do
    result <- genContestTx numParties
    case result of
      Nothing -> pure Nothing
      Just (tx, utxo) ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx utxo

  genContestTx :: Int -> Gen (Maybe (Tx, UTxO))
  genContestTx numParties = do
    -- Build chain: Init → CollectCom → Close → Contest
    chainResult <- genClosedHeadChain numParties
    case chainResult of
      Nothing -> pure Nothing
      Just (headId, closedThreadOutput, utxo, signingKeys, contestationDeadline) -> do
        -- Generate a newer confirmed snapshot
        snapshotUTxO <- genUTxOAdaOnlyOfSize 1
        let closedSnapshotNumber = 1 -- The close used snapshot 1
            newerSnapshotNumber = 2
        confirmedSnapshot <- genConfirmedSnapshot headId 0 newerSnapshotNumber snapshotUTxO Nothing mempty signingKeys
        contestationPeriod <- arbitrary

        -- Generate point in time before contestation deadline
        pointInTime <- genPointInTimeBefore contestationDeadline

        scriptRegistry <- genScriptRegistry
        vk <- genVerificationKey

        let tx = case confirmedSnapshot of
              ConfirmedSnapshot{snapshot, signatures} ->
                contestTx scriptRegistry vk headId contestationPeriod 0 snapshot signatures pointInTime closedThreadOutput NoThing
              _ -> error "Expected ConfirmedSnapshot"
            allUTxO = utxo <> UTxO.fromList [closedThreadOutput.closedThreadUTxO]

        pure $ Just (tx, allUTxO)

computeDecrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeDecrementCost = computeCostWithLimit 50 compute
 where
  compute numParties = do
    result <- genDecrementTx numParties
    case result of
      Nothing -> pure Nothing
      Just (tx, utxo) ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx utxo

  genDecrementTx :: Int -> Gen (Maybe (Tx, UTxO))
  genDecrementTx numParties = do
    -- Build chain: Init → CollectCom (open head)
    chainResult <- genOpenHeadChain numParties
    case chainResult of
      Nothing -> pure Nothing
      Just (headId, openThreadOutput, utxo, parties, signingKeys) -> do
        -- Generate a snapshot with decommit UTxO
        snapshotUTxO <- genUTxOAdaOnlyOfSize 1
        utxoToDecommit <- genUTxOAdaOnlyOfSize 1
        let snapshot = Snapshot
              { headId = headId
              , version = 0
              , number = 1
              , confirmed = []
              , utxo = snapshotUTxO
              , utxoToCommit = Nothing
              , utxoToDecommit = Just utxoToDecommit
              }

        -- Generate signatures for the snapshot
        let signatures = genMultiSignature signingKeys snapshot

        scriptRegistry <- genScriptRegistry
        vk <- genVerificationKey
        headParams <- genHeadParameters numParties

        let tx = decrementTx scriptRegistry vk headId headParams openThreadOutput.openThreadUTxO snapshot signatures
            allUTxO = utxo <> UTxO.fromList [openThreadOutput.openThreadUTxO]

        pure $ Just (tx, allUTxO)

computeIncrementCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeIncrementCost = computeCostWithLimit 50 compute
 where
  compute numParties = do
    result <- genIncrementTx numParties
    case result of
      Nothing -> pure Nothing
      Just (tx, utxo) ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx utxo

  genIncrementTx :: Int -> Gen (Maybe (Tx, UTxO))
  genIncrementTx numParties = do
    -- Build chain: Init → CollectCom (open head)
    chainResult <- genOpenHeadChain numParties
    case chainResult of
      Nothing -> pure Nothing
      Just (headId, openThreadOutput, utxo, parties, signingKeys) -> do
        -- Generate a snapshot with commit UTxO
        snapshotUTxO <- genUTxOAdaOnlyOfSize 1
        utxoToCommit <- genUTxOAdaOnlyOfSize 1
        let snapshot = Snapshot
              { headId = headId
              , version = 0
              , number = 1
              , confirmed = []
              , utxo = snapshotUTxO
              , utxoToCommit = Just utxoToCommit
              , utxoToDecommit = Nothing
              }

        -- Generate signatures for the snapshot
        let signatures = genMultiSignature signingKeys snapshot

        -- For benchmarking, create a mock deposit output
        depositIn <- genTxIn
        depositOut <- genOutputFor =<< arbitrary
        let depositUTxO = UTxO.singleton depositIn depositOut

        scriptRegistry <- genScriptRegistry
        vk <- genVerificationKey
        headParams <- genHeadParameters numParties
        upperValiditySlot <- arbitrary :: Gen SlotNo

        let tx = incrementTx scriptRegistry vk headId headParams openThreadOutput.openThreadUTxO snapshot depositUTxO upperValiditySlot signatures
            allUTxO = utxo <> UTxO.fromList [openThreadOutput.openThreadUTxO] <> depositUTxO

        pure $ Just (tx, allUTxO)

computeAbortCost :: Gen [(NumParties, TxSize, MemUnit, CpuUnit, Coin)]
computeAbortCost =
  catMaybes <$> forM [1 .. 100] compute
 where
  compute numParties = do
    result <- genAbortTx numParties
    case result of
      Nothing -> pure Nothing
      Just (tx, utxo) ->
        evaluateAndMakeCostTuple (\txSize memUnit cpuUnit minFee -> (NumParties numParties, txSize, memUnit, cpuUnit, minFee)) tx utxo

  genAbortTx :: Int -> Gen (Maybe (Tx, UTxO))
  genAbortTx numParties = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    scriptRegistry <- genScriptRegistry

    -- Generate init transaction
    seedInput <- genTxIn
    participants <- replicateM numParties (arbitrary :: Gen OnChainId)
    headParameters <- genHeadParameters numParties
    let initTx' = initTx networkId seedInput participants headParameters

    case observeInitTx initTx' of
      Left _err -> pure Nothing
      Right observation -> do
        let headSeed = observation.headSeed
            initPairs = txOutputsToUTxOPairs initTx'

        case initPairs of
          [] -> pure Nothing
          (headPair : initialPairs) -> do
            vk <- genVerificationKey
            -- For abort, we need some commits - generate simplified ones
            let spendableUTxO = UTxO.fromList initPairs
                initials = Map.fromList initialPairs
                commits = Map.empty -- Simplified: no actual commits for benchmarking
                utxoToFanout = mempty
                headTokenScript = mkHeadTokenScript seedInput
                tx = case abortTx utxoToFanout scriptRegistry vk headPair headTokenScript initials commits of
                  Right t -> t
                  Left _ -> error "Failed to create abort tx"

            pure $ Just (tx, spendableUTxO)

computeFanOutCost :: Gen [(NumParties, NumUTxO, Natural, TxSize, MemUnit, CpuUnit, Coin)]
computeFanOutCost = do
  interesting <- catMaybes <$> mapM (uncurry compute) [(p, u) | p <- [numberOfParties], u <- [0, 1, 5, 10, 20, 30, 40, 50]]
  limit <-
    maybeToList
      . getFirst
      <$> foldMapM
        (\(p, u) -> First <$> compute p u)
        [(p, u) | p <- [numberOfParties], u <- [100, 99 .. 0]]
  pure $ interesting <> limit
 where
  numberOfParties = 10

  compute parties numElems = do
    result <- genFanoutTx parties numElems
    case result of
      Nothing -> pure Nothing
      Just (utxo, tx, knownUTxO) -> do
        let utxoSerializedSize = serializedSize utxo
        case checkSizeAndEvaluate tx knownUTxO of
          Just (txSize, memUnit, cpuUnit, minFee) ->
            pure $ Just (NumParties parties, NumUTxO numElems, utxoSerializedSize, txSize, memUnit, cpuUnit, minFee)
          Nothing ->
            pure Nothing

  genFanoutTx :: Int -> Int -> Gen (Maybe (UTxO, Tx, UTxO))
  genFanoutTx numParties numOutputs = do
    utxo <- genUTxOAdaOnlyOfSize numOutputs
    chainResult <- genClosedHeadChain numParties
    case chainResult of
      Nothing -> pure Nothing
      Just (_headId, closedThreadOutput, knownUTxO, _signingKeys, contestationDeadline) -> do
        let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength contestationDeadline

        scriptRegistry <- genScriptRegistry
        seedTxIn <- genTxIn

        let headTokenScript = mkHeadTokenScript seedTxIn
            tx = fanoutTx scriptRegistry utxo Nothing Nothing closedThreadOutput.closedThreadUTxO deadlineSlotNo headTokenScript
            allUTxO = knownUTxO <> UTxO.fromList [closedThreadOutput.closedThreadUTxO]

        pure $ Just (utxo, tx, allUTxO)

-- * Transaction chain helper functions

-- | Generate a complete open head chain: Init → CollectCom
-- Returns the HeadId, OpenThreadOutput, known UTxO, parties, and signing keys
genOpenHeadChain :: Int -> Gen (Maybe (HeadId, OpenThreadOutput, UTxO, [Party], [SigningKey HydraKey]))
genOpenHeadChain numParties = do
  networkId <- Testnet . NetworkMagic <$> arbitrary
  scriptRegistry <- genScriptRegistry
  vk <- genVerificationKey

  -- Generate init transaction
  seedInput <- genTxIn
  participants <- replicateM numParties (arbitrary :: Gen OnChainId)
  signingKeys <- vector numParties :: Gen [SigningKey HydraKey]
  let parties = map deriveParty signingKeys
  headParameters <- pure $ HeadParameters <$> arbitrary <*> pure parties
  headParams <- headParameters
  let initTx' = initTx networkId seedInput participants headParams

  case observeInitTx initTx' of
    Left _err -> pure Nothing
    Right observation -> do
      let headId = observation.headId
          initPairs = txOutputsToUTxOPairs initTx'

      -- Generate commit transactions (simplified for benchmarking)
      utxosToCommit <- replicateM numParties (genUTxOAdaOnlyOfSize 1)
      let utxoToCollect = fold utxosToCommit

      case initPairs of
        [] -> pure Nothing
        (headPair : initialPairs) -> do
          let commits = Map.fromList initialPairs
              knownUTxO = UTxO.fromList (headPair : initialPairs)
              collectComTx' = collectComTx networkId scriptRegistry vk headId headParams headPair commits utxoToCollect

          -- Extract OpenThreadOutput from CollectCom transaction
          let collectComPairs = txOutputsToUTxOPairs collectComTx'

          case collectComPairs of
            [] -> pure Nothing
            (openHeadPair : _) -> do
              -- Build OpenThreadOutput
              -- Need to extract parties and contestation period from datum
              let openThreadOutput = OpenThreadOutput
                    { openThreadUTxO = openHeadPair
                    , openContestationPeriod = toChain headParams.contestationPeriod
                    , openParties = map partyToChain parties
                    }
                  allUTxO = knownUTxO

              pure $ Just (headId, openThreadOutput, allUTxO, parties, signingKeys)

-- | Generate a complete closed head chain: Init → CollectCom → Close
-- Returns the HeadId, ClosedThreadOutput, known UTxO, signing keys, and contestation deadline
genClosedHeadChain :: Int -> Gen (Maybe (HeadId, ClosedThreadOutput, UTxO, [SigningKey HydraKey], UTCTime))
genClosedHeadChain numParties = do
  openChainResult <- genOpenHeadChain numParties
  case openChainResult of
    Nothing -> pure Nothing
    Just (headId, openThreadOutput, utxo, parties, signingKeys) -> do
      -- Generate a confirmed snapshot
      snapshotUTxO <- genUTxOAdaOnlyOfSize 1
      snapshot <- genConfirmedSnapshot headId 0 1 snapshotUTxO Nothing mempty signingKeys

      -- Generate Close transaction
      scriptRegistry <- genScriptRegistry
      vk <- genVerificationKey
      contestationPeriod <- arbitrary :: Gen ContestationPeriod
      (startSlot, pointInTime@(_, utcTime)) <- genValidityBoundsFromContestationPeriod contestationPeriod

      let closeTx' = closeTx scriptRegistry vk headId 0 snapshot startSlot pointInTime openThreadOutput NoThing

      -- Extract ClosedThreadOutput from Close transaction
      let closePairs = txOutputsToUTxOPairs closeTx'

      case closePairs of
        [] -> pure Nothing
        (closedHeadPair : _) -> do
          -- Calculate contestation deadline
          let contestationDeadline = addUTCTime (fromIntegral contestationPeriod) utcTime

          -- Build ClosedThreadOutput
          let closedThreadOutput = ClosedThreadOutput
                { closedThreadUTxO = closedHeadPair
                , closedParties = map partyToChain parties
                , closedContestationDeadline = fromIntegral $ (`div` 1000000) $ floor $ utcTimeToPOSIXSeconds contestationDeadline * 1000000
                , closedContesters = [] -- No contesters yet
                }
              allUTxO = utxo

          pure $ Just (headId, closedThreadOutput, allUTxO, signingKeys, contestationDeadline)

-- * Helper types

newtype NumParties = NumParties Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype NumUTxO = NumUTxO Int
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype TxSize = TxSize Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype MemUnit = MemUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype CpuUnit = CpuUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

-- * Helper functions

checkSizeAndEvaluate :: Tx -> UTxO -> Maybe (TxSize, MemUnit, CpuUnit, Coin)
checkSizeAndEvaluate tx knownUTxO = do
  guard $ txSize < maxTxSize
  case evaluateTx tx knownUTxO of
    (Right report) -> do
      guard $ all isRight report
      let ExecutionUnits
            { executionMemory = usedMemory
            , executionSteps = usedCpu
            } = usedExecutionUnits report
      let minFee = estimateMinFee tx report
      Just (TxSize txSize, MemUnit usedMemory, CpuUnit usedCpu, minFee)
    _ -> Nothing
 where
  txSize = fromIntegral $ LBS.length $ serialize tx

serializedSize :: UTxO -> Natural
serializedSize =
  fromIntegral
    . lengthOfByteString
    . UTxO.foldMap (serialiseData . toBuiltinData . fromJust . toPlutusTxOut)

genHeadParameters :: Int -> Gen HeadParameters
genHeadParameters n = do
  parties <- replicateM n genParty
  contestationPeriod <- arbitrary
  pure $ HeadParameters contestationPeriod parties

genParty :: Gen Party
genParty = deriveParty <$> (arbitrary :: Gen (SigningKey HydraKey))

-- | Generate a multi-signature for a snapshot from multiple signing keys
genMultiSignature :: [SigningKey HydraKey] -> Snapshot Tx -> MultiSignature (Snapshot Tx)
genMultiSignature signingKeys snapshot =
  aggregate $ map (`sign` snapshot) signingKeys

-- | Convert transaction outputs to UTxO pairs (TxIn, TxOut CtxUTxO)
txOutputsToUTxOPairs :: Tx -> [(TxIn, TxOut CtxUTxO)]
txOutputsToUTxOPairs tx =
  let outputs = txOuts' tx
      txId = getTxId $ getTxBody tx
   in (\(txOut, n) -> (TxIn txId (TxIx n), toCtxUTxOTxOut txOut)) <$> zip outputs [0 ..]

-- | Compute costs with the "interesting + limit" pattern
-- Takes an upper limit and a compute function, returns interesting values [1,2,3,5,10] plus the limit
computeCostWithLimit :: Int -> (Int -> Gen (Maybe a)) -> Gen [a]
computeCostWithLimit upperLimit compute = do
  interesting <- catMaybes <$> mapM compute [1, 2, 3, 5, 10]
  limit <- maybeToList . getFirst <$> foldMapM (fmap First . compute) [upperLimit, upperLimit - 1 .. 11]
  pure $ interesting <> limit

-- | Evaluate a transaction and create a cost tuple with the given wrapper function
evaluateAndMakeCostTuple :: (TxSize -> MemUnit -> CpuUnit -> Coin -> result) -> Tx -> UTxO -> Gen (Maybe result)
evaluateAndMakeCostTuple makeTuple tx utxo =
  case checkSizeAndEvaluate tx utxo of
    Just (txSize, memUnit, cpuUnit, minFee) ->
      pure $ Just (makeTuple txSize memUnit cpuUnit minFee)
    Nothing ->
      pure Nothing
