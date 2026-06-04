{-# LANGUAGE DuplicateRecordFields #-}

-- | End-to-end regression tests for the deposit-stealing class of bugs.
module Hydra.Cluster.SecurityScenarios where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
 )
import CardanoNode (EndToEndLog (..), runBackend)
import Control.Lens ((^?))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  AssetId (AssetId),
  Coin (..),
  LedgerProtocolParameters (..),
  Quantity (..),
  TxId,
  addTxExtraKeyWits,
  addTxIns,
  addTxInsCollateral,
  addTxOuts,
  chainPointToSlotNo,
  defaultTxBodyContent,
  fromCtxUTxOTxOut,
  fromScriptData,
  lovelaceToValue,
  mkScriptAddress,
  mkScriptWitness,
  mkTxOutDatumInline,
  mkVkAddress,
  modifyTxOutDatum,
  modifyTxOutValue,
  scriptPolicyId,
  scriptWitnessInCtx,
  selectLovelace,
  setTxProtocolParams,
  setTxValidityLowerBound,
  setTxValidityUpperBound,
  signTx,
  toPlutusTxOutRef,
  toScriptData,
  toShelleyNetwork,
  txOutScriptData,
  txOutValue,
  verificationKeyHash,
  pattern BuildTxWith,
  pattern InlineScriptDatum,
  pattern KeyWitness,
  pattern KeyWitnessForSpending,
  pattern PlutusScript,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern ShelleyAddressInEra,
  pattern TxOut,
  pattern TxOutDatumNone,
  pattern TxValidityLowerBound,
  pattern TxValidityUpperBound,
 )
import Hydra.Cardano.Api qualified as CAPI
import Hydra.Chain.Backend (ChainBackend (..), buildTransactionWithBody)
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..), mkTimeHandle)
import Hydra.Cluster.Faucet (seedFromFaucet, seedFromFaucetWithMinting)
import Hydra.Cluster.Fixture (Actor (..), alice, aliceSk)
import Hydra.Cluster.Scenarios (
  headIsOpenWith,
  refuelIfNeeded,
  returnFundsToFaucet,
 )
import Hydra.Cluster.Util (Timing, chainConfigFor, depositTimeout, keysFor, mkTestTiming, setNetworkId)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState (
  CloseRedeemer (CloseInitial),
  ClosedDatum (ClosedDatum),
  IncrementRedeemer (IncrementRedeemer),
  Input (Close, Increment),
  OpenDatum (OpenDatum),
  State (Closed, Open),
 )
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Logging (Tracer)
import Hydra.Options (ChainBackendOptions (..))
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (HeadId, IsTx (balance), headIdToCurrencySymbol, headIdToPolicyId, txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregate, sign, toPlutusSignatures)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Snapshot (Snapshot (Snapshot))
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.Tx.Utils (hydraHeadV2AssetName)
import HydraNode (
  HydraClient,
  input,
  requestCommitTx,
  send,
  waitMatch,
  withSoloHydraNode,
 )
import PlutusLedgerApi.V3 (toBuiltin)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)

-- * Standalone Claim with no head input

-- | Regression test for the standalone-Claim deposit-stealing attack.
--
-- A third party submits a standalone tx that spends a pending deposit UTxO
-- with the @Claim@ redeemer, redirecting the value to their own address.
-- The tx has no Head input at all, so the deposit validator's @Claim@
-- branch fails on @list.find@ with @DepositHeadInputNotFound@ (D06).
cannotStealDepositWithoutHeadInput ::
  Tracer IO EndToEndLog -> FilePath -> ChainBackendOptions -> [TxId] -> IO ()
cannotStealDepositWithoutHeadInput tracer workDir opts hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer opts Alice) $ do
    refuelIfNeeded tracer opts Alice 30_000_000
    blockTime <- runBackend opts getBlockTime
    let timing = mkTestTiming blockTime
    networkId <- runBackend opts queryNetworkId
    aliceChainConfig <-
      chainConfigFor Alice workDir opts hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer
    withSoloHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] $ \n1 -> do
      send n1 $ input "Init" []
      _headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      (victimVk, depositTxId') <- placeVictimDeposit tracer opts n1 timing 5_000_000

      -- Kate: independent attacker wallet, funded for fees + collateral.
      (kateVk, kateSk) <- generate genKeyPair
      let kateFunds = 30_000_000 :: Integer
      kateFundsUTxO <-
        seedFromFaucet opts kateVk (lovelaceToValue (fromInteger kateFunds)) (contramap FromFaucet tracer)

      depositUTxO <-
        runBackend opts $ queryUTxOByTxIn [CAPI.TxIn depositTxId' (CAPI.TxIx 0)]
      (depositTxIn, depositTxOut) <- requireSingletonUTxO "deposit UTxO" depositUTxO
      (kateTxIn, _) <- requireSingletonUTxO "Kate funding UTxO" kateFundsUTxO

      let kateAddr = mkVkAddress networkId kateVk
          stolenValue = txOutValue depositTxOut
          stealOutput = TxOut kateAddr stolenValue TxOutDatumNone ReferenceScriptNone
          extraInputs =
            [(kateTxIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)]

      attemptStealAndAssertRejected
        AttemptArgs
          { tracer
          , opts
          , depositTxIn
          , depositTxOuts = [(depositTxIn, depositTxOut)]
          , extraInputs
          , collateralIn = kateTxIn
          , extraOutputs = [stealOutput]
          , attackerSk = kateSk
          , attackerAddr = kateAddr
          , spendable = depositUTxO <> kateFundsUTxO
          , -- No head input is consumed at all, so the deposit's Claim
            -- branch fails on @list.find@ with @DepositHeadInputNotFound@.
            expectedTrace = toString (toErrorCode DepositHeadInputNotFound)
          }

      assertDepositStillLocked opts depositTxId' stolenValue
      assertWalletLovelace opts victimVk 0
      assertWalletLovelace opts kateVk kateFunds

-- * Counterfeit Head state-token

-- | The attacker mints a counterfeit token of asset name @HydraHeadV2@
-- under a permissive minting policy they control, locks it in their own
-- wallet, and uses that UTxO as bait alongside a victim deposit. The
-- deposit validator's @Claim@ branch searches the inputs for a UTxO that
-- carries the real head's policy id; the counterfeit token's policy id
-- does not match, so the validator fails with
-- @DepositHeadInputNotFound@ (D06).
cannotStealDepositWithCounterfeitHeadToken ::
  Tracer IO EndToEndLog -> FilePath -> ChainBackendOptions -> [TxId] -> IO ()
cannotStealDepositWithCounterfeitHeadToken tracer workDir opts hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer opts Alice) $ do
    refuelIfNeeded tracer opts Alice 30_000_000
    blockTime <- runBackend opts getBlockTime
    let timing = mkTestTiming blockTime
    networkId <- runBackend opts queryNetworkId
    aliceChainConfig <-
      chainConfigFor Alice workDir opts hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer
    withSoloHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] $ \n1 -> do
      send n1 $ input "Init" []
      _headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      (victimVk, depositTxId') <- placeVictimDeposit tracer opts n1 timing 5_000_000

      -- Kate mints a counterfeit (mu_attack, "HydraHeadV2") token under
      -- a permissive minting policy and locks it (alongside enough ada for
      -- fees + collateral) in her own wallet. mu_attack is the policy id
      -- of dummyMintingScript and is *not* the policy id of any real Hydra
      -- head -- those are parameterised by a unique seed input.
      (kateVk, kateSk) <- generate genKeyPair
      let kateFunds = 30_000_000 :: Integer
          attackPolicyId = scriptPolicyId (PlutusScript dummyMintingScript)
          counterfeitToken = AssetId attackPolicyId hydraHeadV2AssetName
          counterfeitValue =
            lovelaceToValue (fromInteger kateFunds)
              <> fromList [(counterfeitToken, Quantity 1)]
      kateUTxO <-
        seedFromFaucetWithMinting
          opts
          kateVk
          counterfeitValue
          (contramap FromFaucet tracer)
          (Just dummyMintingScript)

      depositUTxO <-
        runBackend opts $ queryUTxOByTxIn [CAPI.TxIn depositTxId' (CAPI.TxIx 0)]
      (depositTxIn, depositTxOut) <- requireSingletonUTxO "deposit UTxO" depositUTxO
      (kateTxIn, _) <- requireSingletonUTxO "Kate bait UTxO" kateUTxO

      let kateAddr = mkVkAddress networkId kateVk
          stolenValue = txOutValue depositTxOut
          stealOutput = TxOut kateAddr stolenValue TxOutDatumNone ReferenceScriptNone

      attemptStealAndAssertRejected
        AttemptArgs
          { tracer
          , opts
          , depositTxIn
          , depositTxOuts = [(depositTxIn, depositTxOut)]
          , extraInputs =
              [(kateTxIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
          , collateralIn = kateTxIn
          , extraOutputs = [stealOutput]
          , attackerSk = kateSk
          , attackerAddr = kateAddr
          , spendable = depositUTxO <> kateUTxO
          , -- The counterfeit token's policy id does not match the real
            -- head's policy id, so @list.find@ in the deposit Claim branch
            -- fails to identify any head input.
            expectedTrace = toString (toErrorCode DepositHeadInputNotFound)
          }

      assertDepositStillLocked opts depositTxId' stolenValue
      assertWalletLovelace opts victimVk 0
      -- Kate's UTxO is unchanged; her ada balance is still her seed.
      assertWalletLovelace opts kateVk kateFunds

-- * Helpers

-- | Place a deposit of `amount` lovelace from a fresh wallet into the head
-- managed by @n1@. Returns the wallet's verification key (so its balance
-- can be queried later) and the deposit transaction id.
placeVictimDeposit ::
  Tracer IO EndToEndLog ->
  ChainBackendOptions ->
  HydraClient ->
  Timing ->
  Integer ->
  IO (CAPI.VerificationKey CAPI.PaymentKey, TxId)
placeVictimDeposit tracer opts n1 timing amount = do
  (vk, sk) <- generate genKeyPair
  utxo <- seedFromFaucet opts vk (lovelaceToValue (fromInteger amount)) (contramap FromFaucet tracer)
  depositTx <- requestCommitTx n1 utxo <&> signTx sk
  runBackend opts $ submitTransaction depositTx
  _ :: UTCTime <- waitMatch (depositTimeout timing) n1 $ \v -> do
    guard $ v ^? key "tag" == Just "CommitRecorded"
    v ^? key "deadline" >>= parseMaybe parseJSON
  pure (vk, txId depositTx)

-- | Encapsulates the moving parts of an attempted-steal transaction.
data AttemptArgs = AttemptArgs
  { tracer :: Tracer IO EndToEndLog
  , opts :: ChainBackendOptions
  , depositTxIn :: CAPI.TxIn
  -- ^ Diagnostic / naming hook for the "primary" deposit being attacked.
  , depositTxOuts :: [(CAPI.TxIn, CAPI.TxOut CAPI.CtxUTxO)]
  -- ^ Deposit-script UTxOs being attacked. The first entry is wired up
  -- as a Claim-witnessed input by the helper; additional deposit inputs
  -- (for amplification cases) must be passed through 'extraInputs' with
  -- their own deposit-script witness.
  -- TODO: This may not need to be alist, and should be more simply a "UTxO"
  -- type.
  , extraInputs :: [(CAPI.TxIn, CAPI.BuildTxWith CAPI.BuildTx (CAPI.Witness CAPI.WitCtxTxIn))]
  -- ^ Additional non-deposit-script inputs (key witnesses or extra deposit
  -- script witnesses for amplification cases).
  , collateralIn :: CAPI.TxIn
  , extraOutputs :: [CAPI.TxOut CAPI.CtxTx]
  , attackerSk :: CAPI.SigningKey CAPI.PaymentKey
  , attackerAddr :: CAPI.AddressInEra
  , spendable :: CAPI.UTxO
  , expectedTrace :: String
  -- ^ Plutus error trace (e.g. @"D06"@) expected on the autobalancer's
  -- 'Left'. The build error must contain this trace; any other 'Left'
  -- (insufficient ada for fees, integrity-hash mismatch, era hiccup, ...)
  -- is treated as a test failure rather than success.
  }

-- | Build a steal transaction and assert the autobalancer rejects it
-- specifically because the deposit validator failed with
-- 'AttemptArgs.expectedTrace'. Any other 'Left' (insufficient ada,
-- integrity hash mismatch, era issue, ...) is treated as a test failure.
attemptStealAndAssertRejected :: AttemptArgs -> IO ()
attemptStealAndAssertRejected
  AttemptArgs
    { opts
    , depositTxOuts
    , extraInputs
    , collateralIn
    , extraOutputs
    , attackerSk = _
    , attackerAddr
    , spendable
    , expectedTrace
    } = do
    let claimRedeemer = toScriptData $ Deposit.redeemer Deposit.Claim
        depositWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness depositValidatorScript CAPI.InlineScriptDatum claimRedeemer
        firstDepositInput =
          case depositTxOuts of
            ((i, _) : _) -> (i, depositWitness)
            [] -> error "attemptStealAndAssertRejected: no deposit inputs"

    pparams <- runBackend opts $ queryProtocolParameters QueryTip
    systemStart <- runBackend opts $ querySystemStart QueryTip
    eraHistory <- runBackend opts $ queryEraHistory QueryTip
    stakePools <- runBackend opts $ queryStakePools QueryTip
    tip <- runBackend opts queryTip
    let tipSlot = fromMaybe 0 (chainPointToSlotNo tip)
        -- Validity bound well before the (~3 * 20 * blockTime) deadline.
        -- With default test timing (blockTime = 1s), deadline is ~60s out;
        -- 30 slots is comfortably inside that window.
        upperSlot = tipSlot + 30

    let body =
          defaultTxBodyContent
            & addTxIns (firstDepositInput : extraInputs)
            & addTxOuts extraOutputs
            & addTxInsCollateral [collateralIn]
            & setTxValidityUpperBound (TxValidityUpperBound upperSlot)
            & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

    case buildTransactionWithBody pparams systemStart eraHistory stakePools attackerAddr body spendable of
      Left e -> show e `shouldContain` expectedTrace
      Right _ ->
        error "expected script evaluation to reject the steal tx, but the build succeeded"

requireSingletonUTxO ::
  HasCallStack =>
  String ->
  CAPI.UTxO ->
  IO (CAPI.TxIn, CAPI.TxOut CAPI.CtxUTxO)
requireSingletonUTxO description utxo =
  case UTxO.toList utxo of
    [(i, o)] -> pure (i, o)
    xs -> failure $ "expected exactly one " <> description <> ", got: " <> show (length xs)

assertDepositStillLocked ::
  ChainBackendOptions -> TxId -> CAPI.Value -> IO ()
assertDepositStillLocked opts depositTxId' expectedValue = do
  utxo <- runBackend opts $ queryUTxOByTxIn [CAPI.TxIn depositTxId' (CAPI.TxIx 0)]
  selectLovelace (balance utxo) `shouldBe` selectLovelace expectedValue

assertWalletLovelace ::
  ChainBackendOptions -> CAPI.VerificationKey CAPI.PaymentKey -> Integer -> IO ()
assertWalletLovelace opts vk expected =
  (selectLovelace . balance <$> runBackend opts (queryUTxOFor QueryTip vk))
    `shouldReturn` Coin expected

-- * Multi-deposit redirection during a real Increment

-- | A malicious participant of a head constructs an Increment-shaped
-- transaction that consumes the head's ST + the redeemer-specified
-- deposit + ANOTHER pending deposit for the same head, and routes the
-- extra deposit's value to attacker-controlled outputs.
--
-- Setup: 1-party head (Alice). Two unrelated depositors (V1 and V2) each
-- deposit funds for Alice's head. Alice (the malicious participant)
-- constructs an Increment that claims V1's deposit (which is in the
-- snapshot Alice signs) but ALSO consumes V2's deposit and routes its
-- value to a Kate address. The head validator's @mustPreserveValue@
-- sums every non-head input and requires
-- @head_in + Σ non-head inputs == head_out@, so V2's value escaping to Kate
-- makes the equation fail and the tx is rejected with
-- @HeadValueIsNotPreserved@ (H4).
cannotRedirectExtraDepositDuringIncrement ::
  Tracer IO EndToEndLog -> FilePath -> ChainBackendOptions -> [TxId] -> IO ()
cannotRedirectExtraDepositDuringIncrement tracer workDir opts hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer opts Alice) $ do
    refuelIfNeeded tracer opts Alice 60_000_000
    blockTime <- runBackend opts getBlockTime
    let timing = mkTestTiming blockTime
    networkId <- runBackend opts queryNetworkId
    aliceChainConfig <-
      chainConfigFor Alice workDir opts hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer

    -- Use the hydra-node only to bootstrap the head and produce the two
    -- deposit txs. We exit the bracket immediately after submitting the
    -- deposits (without waiting for CommitRecorded), so the node does not
    -- get a chance to submit its own honest Increment that would race the
    -- redirection.
    (headId, victim1Vk, deposit1TxId, victim2Vk, deposit2TxId) <-
      withSoloHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] $ \n1 -> do
        send n1 $ input "Init" []
        hid <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
        (vVk1, dTxId1) <- placeVictimDepositNoWait tracer opts n1 5_000_000
        (vVk2, dTxId2) <- placeVictimDepositNoWait tracer opts n1 7_000_000
        pure (hid, vVk1, dTxId1, vVk2, dTxId2)

    -- Wait for the deposit script UTxOs to land on chain.
    deposit1UTxO <- waitForOnChainUTxO opts (CAPI.TxIn deposit1TxId (CAPI.TxIx 0))
    deposit2UTxO <- waitForOnChainUTxO opts (CAPI.TxIn deposit2TxId (CAPI.TxIx 0))
    (deposit1In, deposit1Out) <- requireSingletonUTxO "deposit 1 UTxO" deposit1UTxO
    (deposit2In, deposit2Out) <- requireSingletonUTxO "deposit 2 UTxO" deposit2UTxO

    -- Fee/collateral wallet for Alice's malicious tx (using AliceFunds —
    -- separate from the participation key used for the head's
    -- mustBeSignedByParticipant check).
    (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
    (aliceFundsVk, _aliceFundsSk) <- keysFor AliceFunds
    aliceFundsUTxO <-
      seedFromFaucet
        opts
        aliceFundsVk
        (lovelaceToValue 30_000_000)
        (contramap FromFaucet tracer)
    (aliceFundsIn, _) <- requireSingletonUTxO "Alice funds" aliceFundsUTxO

    -- Kate is the recipient of the redirected funds — could be Alice
    -- herself; using a fresh wallet just to make the post-attack assertion
    -- crisp ("Kate had nothing before; if she has D2's value after, the
    -- attack succeeded").
    (kateVk, _) <- generate genKeyPair
    let kateAddr = mkVkAddress networkId kateVk

    -- Locate the head's continuation UTxO and read its OpenDatum.
    (headIn, headOut, prevOpenDatum) <-
      findHeadContinuationUTxO opts networkId headId
    let OpenDatum
          { Head.headSeed = prevHeadSeed
          , Head.parties = prevParties
          , Head.contestationPeriod = prevPeriod
          , Head.version = prevVersion
          } = prevOpenDatum

    -- Decode D1's commits so we can build a snapshot whose
    -- utxoToCommit matches what the on-chain check expects.
    let network = toShelleyNetwork networkId
    d1Commits <- readDepositCommits deposit1Out
    utxoToCommit <-
      case traverse (Commit.deserializeCommit network) d1Commits of
        Just outs -> pure $ UTxO.fromList outs
        Nothing -> failure "failed to deserialize D1 commits"

    -- Construct and sign the snapshot covering ONLY D1.
    let snapshot =
          Snapshot
            { Snapshot.headId = headId
            , Snapshot.version = fromIntegral prevVersion
            , Snapshot.number = fromIntegral (prevVersion + 1)
            , Snapshot.confirmed = []
            , Snapshot.utxo = mempty :: CAPI.UTxO
            , Snapshot.utxoToCommit = Just utxoToCommit
            , Snapshot.utxoToDecommit = Nothing
            , Snapshot.accumulator = Accumulator.buildFromSnapshotUTxOs @CAPI.Tx mempty (Just utxoToCommit) Nothing
            }
        sigs = aggregate [sign aliceSk snapshot]

    pparams <- runBackend opts $ queryProtocolParameters QueryTip
    systemStart <- runBackend opts $ querySystemStart QueryTip
    eraHistory <- runBackend opts $ queryEraHistory QueryTip
    stakePools <- runBackend opts $ queryStakePools QueryTip
    tip <- runBackend opts queryTip
    let tipSlot = fromMaybe 0 (chainPointToSlotNo tip)
        upperSlot = tipSlot + 30

    let headRedeemer =
          toScriptData $
            Increment
              IncrementRedeemer
                { Head.signature = toPlutusSignatures sigs
                , Head.snapshotNumber = prevVersion + 1
                , Head.increment = toPlutusTxOutRef deposit1In
                }
        headWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness Head.validatorScript InlineScriptDatum headRedeemer

        claimRedeemer = toScriptData $ Deposit.redeemer Deposit.Claim
        depositWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness depositValidatorScript InlineScriptDatum claimRedeemer

        nextOpenDatum =
          OpenDatum
            { Head.headSeed = prevHeadSeed
            , Head.headId = headIdToCurrencySymbol headId
            , Head.parties = prevParties
            , Head.contestationPeriod = prevPeriod
            , Head.version = prevVersion + 1
            , Head.utxoHash = toBuiltin $ hashUTxO @CAPI.Tx (mempty :: CAPI.UTxO)
            , Head.accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash $ Accumulator.buildFromSnapshotUTxOs @CAPI.Tx mempty (Just utxoToCommit) Nothing
            }
        headOut' =
          fromCtxUTxOTxOut headOut
            & modifyTxOutDatum (const $ mkTxOutDatumInline (Open nextOpenDatum))
            & modifyTxOutValue (<> txOutValue deposit1Out)

        redirectedOut =
          TxOut kateAddr (txOutValue deposit2Out) TxOutDatumNone ReferenceScriptNone

        body =
          defaultTxBodyContent
            & addTxIns
              [ (headIn, headWitness)
              , (deposit1In, depositWitness)
              , (deposit2In, depositWitness)
              , (aliceFundsIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
              ]
            & addTxOuts [headOut', redirectedOut]
            & addTxInsCollateral [aliceFundsIn]
            & addTxExtraKeyWits [verificationKeyHash aliceCardanoVk]
            & setTxValidityUpperBound (TxValidityUpperBound upperSlot)
            & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

        spendable :: CAPI.UTxO
        spendable =
          UTxO.singleton headIn headOut
            <> deposit1UTxO
            <> deposit2UTxO
            <> aliceFundsUTxO
        aliceFundsAddr = mkVkAddress networkId aliceFundsVk

    -- Attempt the redirection. The head validator's mustPreserveValue
    -- sums every non-head input and requires
    -- head_in + Σ non-head inputs == head_out. With D2 routed to a non-Head
    -- output, the equation fails and the tx is rejected at the head-side
    -- with HeadValueIsNotPreserved (H4).
    let expectedTrace = toString (toErrorCode HeadValueIsNotPreserved)
    case buildTransactionWithBody pparams systemStart eraHistory stakePools aliceFundsAddr body spendable of
      Left e ->
        show e `shouldContain` expectedTrace
      Right _ ->
        error "expected script evaluation to reject the redirection tx, but the build succeeded"

    -- Both deposits remain locked at v_deposit; Kate's address has nothing.
    assertDepositStillLocked opts deposit1TxId (txOutValue deposit1Out)
    assertDepositStillLocked opts deposit2TxId (txOutValue deposit2Out)
    assertWalletLovelace opts victim1Vk 0
    assertWalletLovelace opts victim2Vk 0
    assertWalletLovelace opts kateVk 0

-- * Increment-redirection helpers

-- | Like 'placeVictimDeposit' but does not wait for the hydra-node to
-- emit @CommitRecorded@. The caller is expected to exit the
-- 'withHydraNode' bracket promptly so the node cannot submit its own
-- honest Increment for the deposit.
placeVictimDepositNoWait ::
  Tracer IO EndToEndLog ->
  ChainBackendOptions ->
  HydraClient ->
  Integer ->
  IO (CAPI.VerificationKey CAPI.PaymentKey, TxId)
placeVictimDepositNoWait tracer opts n1 amount = do
  (vk, sk) <- generate genKeyPair
  utxo <- seedFromFaucet opts vk (lovelaceToValue (fromInteger amount)) (contramap FromFaucet tracer)
  depositTx <- requestCommitTx n1 utxo <&> signTx sk
  runBackend opts $ submitTransaction depositTx
  pure (vk, txId depositTx)

-- | Poll the chain for a UTxO at the given TxIn until it appears.
-- Times out after ~30 seconds.
waitForOnChainUTxO :: ChainBackendOptions -> CAPI.TxIn -> IO CAPI.UTxO
waitForOnChainUTxO opts txIn = go (60 :: Int)
 where
  go 0 = failure $ "UTxO " <> show txIn <> " never appeared on chain"
  go n = do
    utxo <- runBackend opts $ queryUTxOByTxIn [txIn]
    if UTxO.null utxo
      then threadDelay 0.5 >> go (n - 1)
      else pure utxo

-- | Locate the head's continuation UTxO at v_head address, identified
-- by its state token policy id matching the head id.
findHeadContinuationUTxO ::
  ChainBackendOptions ->
  CAPI.NetworkId ->
  HeadId ->
  IO (CAPI.TxIn, CAPI.TxOut CAPI.CtxUTxO, OpenDatum)
findHeadContinuationUTxO opts networkId headId = do
  let headAddr =
        case mkScriptAddress networkId Head.validatorScript of
          ShelleyAddressInEra addr -> addr
          _ -> error "head address is not Shelley"
  headPolicyId <- maybe (failure "head id is not a valid policy id") pure (headIdToPolicyId headId)
  allHeadUTxOs <- runBackend opts $ queryUTxO [headAddr]
  let matches =
        UTxO.filter
          ( \o ->
              CAPI.selectAsset (txOutValue o) (AssetId headPolicyId hydraHeadV2AssetName)
                == Quantity 1
          )
          allHeadUTxOs
  (headIn, headOut) <- requireSingletonUTxO "head continuation UTxO" matches
  case txOutScriptData (fromCtxUTxOTxOut headOut) of
    Nothing -> failure "head UTxO has no inline datum"
    Just sd -> case fromScriptData sd of
      Just (Open openDatum) -> pure (headIn, headOut, openDatum)
      _ -> failure "head UTxO datum is not Open"

-- | Read the @[Commit]@ list from a deposit-script UTxO's inline datum.
readDepositCommits :: CAPI.TxOut CAPI.CtxUTxO -> IO [Commit.Commit]
readDepositCommits depositOut =
  case txOutScriptData (fromCtxUTxOTxOut depositOut) of
    Nothing -> failure "deposit UTxO has no inline datum"
    Just sd -> case fromScriptData sd :: Maybe Deposit.DepositDatum of
      Just (_, _, commits) -> pure commits
      Nothing -> failure "deposit UTxO datum doesn't decode"

-- * Close-with-deposit absorption

-- | A malicious head participant closes the head with a transaction
-- that ALSO spends a foreign deposit input, padding the closed-state
-- continuation's value with the deposit's value (mustPreserveHeadValue
-- uses 'geq', so head_out > head_in is allowed). After the contestation
-- deadline they fanout the absorbed value to their own address. The
-- deposit validator's @head_output_is_open@ check requires the head's
-- continuation datum to be the Open state, so Close / Contest / Fanout
-- transitions cannot absorb deposits and the tx is rejected with
-- @HeadOutputNotOpen@ (D06).
--
-- The test uses the simplest of the close redeemers, 'CloseInitial',
-- which doesn't require an off-chain snapshot signature: just
-- snapshotNumber = 0 and utxoHash = the open's initialUtxoHash.
cannotAbsorbDepositDuringClose ::
  Tracer IO EndToEndLog -> FilePath -> ChainBackendOptions -> [TxId] -> IO ()
cannotAbsorbDepositDuringClose tracer workDir opts hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer opts Alice) $ do
    refuelIfNeeded tracer opts Alice 60_000_000
    blockTime <- runBackend opts getBlockTime
    let timing = mkTestTiming blockTime
    networkId <- runBackend opts queryNetworkId
    aliceChainConfig <-
      chainConfigFor Alice workDir opts hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer

    -- Bootstrap the head and craft the deposit tx; exit before the node
    -- can auto-Increment.
    (headId, victimVk, depositTxId') <-
      withSoloHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] $ \n1 -> do
        send n1 $ input "Init" []
        hid <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
        (vVk, dTxId) <- placeVictimDepositNoWait tracer opts n1 5_000_000
        pure (hid, vVk, dTxId)

    depositUTxO <- waitForOnChainUTxO opts (CAPI.TxIn depositTxId' (CAPI.TxIx 0))
    (depositTxIn, depositTxOut) <- requireSingletonUTxO "deposit UTxO" depositUTxO

    (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
    (aliceFundsVk, _aliceFundsSk) <- keysFor AliceFunds
    aliceFundsUTxO <-
      seedFromFaucet
        opts
        aliceFundsVk
        (lovelaceToValue 30_000_000)
        (contramap FromFaucet tracer)
    (aliceFundsIn, _) <- requireSingletonUTxO "Alice funds" aliceFundsUTxO

    (headIn, headOut, prevOpenDatum) <-
      findHeadContinuationUTxO opts networkId headId
    let OpenDatum
          { Head.headSeed = _prevHeadSeed
          , Head.parties = prevParties
          , Head.contestationPeriod = prevPeriod
          , Head.version = prevVersion
          } = prevOpenDatum

    pparams <- runBackend opts $ queryProtocolParameters QueryTip
    systemStart <- runBackend opts $ querySystemStart QueryTip
    eraHistory <- runBackend opts $ queryEraHistory QueryTip
    stakePools <- runBackend opts $ queryStakePools QueryTip
    tip <- runBackend opts queryTip
    let tipSlot = fromMaybe 0 (chainPointToSlotNo tip)
        -- Keep the validity range tight (<= contestationPeriod). With test
        -- timing of contestationPeriod = 2s and slot length 1s,
        -- 1-slot range is well within bounds.
        lowerSlot = tipSlot
        upperSlot = tipSlot + 1
        timeHandle = mkTimeHandle tipSlot systemStart eraHistory
    upperUTC <- either (failure . toString) pure (slotToUTCTime timeHandle upperSlot)
    let upperPosix = posixFromUTCTime upperUTC
        contestationDeadline = addContestationPeriod upperPosix prevPeriod

    let closedDatum =
          ClosedDatum
            { Head.headId = headIdToCurrencySymbol headId
            , Head.parties = prevParties
            , Head.contestationPeriod = prevPeriod
            , Head.version = prevVersion
            , Head.snapshotNumber = 0
            , Head.contesters = []
            , Head.contestationDeadline = contestationDeadline
            , Head.accumulatorCommitment = Accumulator.getAccumulatorCommitment $ Accumulator.buildFromSnapshotUTxOs @CAPI.Tx mempty Nothing Nothing
            , Head.headAdaOverhead = 0
            }

        headRedeemer = toScriptData (Close CloseInitial)
        headWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness Head.validatorScript InlineScriptDatum headRedeemer

        claimRedeemer = toScriptData $ Deposit.redeemer Deposit.Claim
        depositWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness depositValidatorScript InlineScriptDatum claimRedeemer

        headOut' =
          fromCtxUTxOTxOut headOut
            & modifyTxOutDatum (const $ mkTxOutDatumInline (Closed closedDatum))
            & modifyTxOutValue (<> txOutValue depositTxOut)

        body =
          defaultTxBodyContent
            & addTxIns
              [ (headIn, headWitness)
              , (depositTxIn, depositWitness)
              , (aliceFundsIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
              ]
            & addTxOuts [headOut']
            & addTxInsCollateral [aliceFundsIn]
            & addTxExtraKeyWits [verificationKeyHash aliceCardanoVk]
            & setTxValidityLowerBound (TxValidityLowerBound lowerSlot)
            & setTxValidityUpperBound (TxValidityUpperBound upperSlot)
            & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

        spendable :: CAPI.UTxO
        spendable =
          UTxO.singleton headIn headOut <> depositUTxO <> aliceFundsUTxO
        aliceFundsAddr = mkVkAddress networkId aliceFundsVk

    case buildTransactionWithBody pparams systemStart eraHistory stakePools aliceFundsAddr body spendable of
      Left _ -> pure ()
      Right _ ->
        error "expected script evaluation to reject the close-with-deposit tx, but the build succeeded"

    -- The deposit is still locked at v_deposit; nothing was redirected.
    assertDepositStillLocked opts depositTxId' (txOutValue depositTxOut)
    assertWalletLovelace opts victimVk 0

-- * Leader self-deposit cover redirection

-- | The malicious snapshot leader places their own SMALL deposit alongside
-- a much LARGER unrelated victim deposit, then constructs an Increment that
-- claims their own small deposit (which is in the snapshot they sign) but
-- ALSO consumes the victim's larger deposit and routes its value to a
-- leader-controlled wallet ("their own pocket") rather than to the head.
--
-- Compared to 'cannotRedirectExtraDepositDuringIncrement' (two unrelated
-- depositors, redirection to a third party), this captures the threat
-- model where the leader benefits *directly* and uses their own honest
-- deposit as cover ("I was just incrementing my deposit"). The asymmetric
-- amounts (small honest, large stolen) make the economic motive explicit.
--
-- The on-chain defense is the same: the head validator's
-- @mustPreserveValue@ sums every non-head input and requires
-- @head_in + Σ non-head inputs == head_out@. With the victim's value
-- routed to a non-Head output, the equation fails and the tx is rejected
-- with @HeadValueIsNotPreserved@ (H4).
--
-- TODO: This could move into the MutationSpec or the TraceSpec.
cannotStealLargerDepositDuringOwnIncrement ::
  Tracer IO EndToEndLog -> FilePath -> ChainBackendOptions -> [TxId] -> IO ()
cannotStealLargerDepositDuringOwnIncrement tracer workDir opts hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer opts Alice) $ do
    refuelIfNeeded tracer opts Alice 60_000_000
    blockTime <- runBackend opts getBlockTime
    let timing = mkTestTiming blockTime
    networkId <- runBackend opts queryNetworkId
    aliceChainConfig <-
      chainConfigFor Alice workDir opts hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer

    -- Bootstrap the head and produce both deposits, exiting before the
    -- node can submit its own honest Increment.
    --
    -- The "leader" deposit is small (3 ADA) and sits in the leader's own
    -- depositor wallet — it is the cover the leader will sign a snapshot
    -- over. The "victim" deposit is much larger (20 ADA) and is what the
    -- leader will redirect.
    (headId, leaderDepositorVk, leaderDepositTxId, victimVk, victimDepositTxId) <-
      withSoloHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] $ \n1 -> do
        send n1 $ input "Init" []
        hid <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
        (lVk, lTxId) <- placeVictimDepositNoWait tracer opts n1 3_000_000
        (vVk, vTxId) <- placeVictimDepositNoWait tracer opts n1 20_000_000
        pure (hid, lVk, lTxId, vVk, vTxId)

    leaderDepositUTxO <- waitForOnChainUTxO opts (CAPI.TxIn leaderDepositTxId (CAPI.TxIx 0))
    victimDepositUTxO <- waitForOnChainUTxO opts (CAPI.TxIn victimDepositTxId (CAPI.TxIx 0))
    (leaderDepositIn, leaderDepositOut) <-
      requireSingletonUTxO "leader deposit UTxO" leaderDepositUTxO
    (victimDepositIn, victimDepositOut) <-
      requireSingletonUTxO "victim deposit UTxO" victimDepositUTxO

    (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
    (aliceFundsVk, _aliceFundsSk) <- keysFor AliceFunds
    aliceFundsUTxO <-
      seedFromFaucet
        opts
        aliceFundsVk
        (lovelaceToValue 30_000_000)
        (contramap FromFaucet tracer)
    (aliceFundsIn, _) <- requireSingletonUTxO "Alice funds" aliceFundsUTxO

    -- Fresh wallet representing "the leader's pocket" — using a wallet
    -- with zero starting balance keeps the post-attack assertion crisp.
    (lootVk, _) <- generate genKeyPair
    let lootAddr = mkVkAddress networkId lootVk

    (headIn, headOut, prevOpenDatum) <-
      findHeadContinuationUTxO opts networkId headId
    let OpenDatum
          { Head.headSeed = prevHeadSeed
          , Head.parties = prevParties
          , Head.contestationPeriod = prevPeriod
          , Head.version = prevVersion
          } = prevOpenDatum

    -- Snapshot covers ONLY the leader's own (small) deposit. This is
    -- what the leader signs; the victim's deposit is silently consumed
    -- by the on-chain tx and never appears in any signed snapshot.
    let network = toShelleyNetwork networkId
    leaderCommits <- readDepositCommits leaderDepositOut
    utxoToCommit <-
      case traverse (Commit.deserializeCommit network) leaderCommits of
        Just outs -> pure $ UTxO.fromList outs
        Nothing -> failure "failed to deserialize leader-deposit commits"

    let snapshot =
          Snapshot
            { Snapshot.headId = headId
            , Snapshot.version = fromIntegral prevVersion
            , Snapshot.number = fromIntegral (prevVersion + 1)
            , Snapshot.confirmed = []
            , Snapshot.utxo = mempty :: CAPI.UTxO
            , Snapshot.utxoToCommit = Just utxoToCommit
            , Snapshot.utxoToDecommit = Nothing
            , Snapshot.accumulator = Accumulator.buildFromSnapshotUTxOs @CAPI.Tx mempty (Just utxoToCommit) Nothing
            }
        sigs = aggregate [sign aliceSk snapshot]

    pparams <- runBackend opts $ queryProtocolParameters QueryTip
    systemStart <- runBackend opts $ querySystemStart QueryTip
    eraHistory <- runBackend opts $ queryEraHistory QueryTip
    stakePools <- runBackend opts $ queryStakePools QueryTip
    tip <- runBackend opts queryTip
    let tipSlot = fromMaybe 0 (chainPointToSlotNo tip)
        upperSlot = tipSlot + 30

    let headRedeemer =
          toScriptData $
            Increment
              IncrementRedeemer
                { Head.signature = toPlutusSignatures sigs
                , Head.snapshotNumber = prevVersion + 1
                , Head.increment = toPlutusTxOutRef leaderDepositIn
                }
        headWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness Head.validatorScript InlineScriptDatum headRedeemer

        claimRedeemer = toScriptData $ Deposit.redeemer Deposit.Claim
        depositWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness depositValidatorScript InlineScriptDatum claimRedeemer

        nextOpenDatum =
          OpenDatum
            { Head.headSeed = prevHeadSeed
            , Head.headId = headIdToCurrencySymbol headId
            , Head.parties = prevParties
            , Head.contestationPeriod = prevPeriod
            , Head.version = prevVersion + 1
            , Head.utxoHash = toBuiltin $ hashUTxO @CAPI.Tx (mempty :: CAPI.UTxO)
            , Head.accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash $ Accumulator.buildFromSnapshotUTxOs @CAPI.Tx mempty (Just utxoToCommit) Nothing
            }
        -- Head value grows by ONLY the leader's small deposit; the
        -- victim's value is siphoned to lootAddr.
        headOut' =
          fromCtxUTxOTxOut headOut
            & modifyTxOutDatum (const $ mkTxOutDatumInline (Open nextOpenDatum))
            & modifyTxOutValue (<> txOutValue leaderDepositOut)

        redirectedOut =
          TxOut lootAddr (txOutValue victimDepositOut) TxOutDatumNone ReferenceScriptNone

        body =
          defaultTxBodyContent
            & addTxIns
              [ (headIn, headWitness)
              , (leaderDepositIn, depositWitness)
              , (victimDepositIn, depositWitness)
              , (aliceFundsIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
              ]
            & addTxOuts [headOut', redirectedOut]
            & addTxInsCollateral [aliceFundsIn]
            & addTxExtraKeyWits [verificationKeyHash aliceCardanoVk]
            & setTxValidityUpperBound (TxValidityUpperBound upperSlot)
            & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

        spendable :: CAPI.UTxO
        spendable =
          UTxO.singleton headIn headOut
            <> leaderDepositUTxO
            <> victimDepositUTxO
            <> aliceFundsUTxO
        aliceFundsAddr = mkVkAddress networkId aliceFundsVk

    let expectedTrace = toString (toErrorCode HeadValueIsNotPreserved)
    case buildTransactionWithBody pparams systemStart eraHistory stakePools aliceFundsAddr body spendable of
      Left e -> show e `shouldContain` expectedTrace
      Right _ ->
        error "expected script evaluation to reject the leader-redirect tx, but the build succeeded"

    -- Both deposits remain locked at v_deposit; the loot wallet has nothing.
    assertDepositStillLocked opts leaderDepositTxId (txOutValue leaderDepositOut)
    assertDepositStillLocked opts victimDepositTxId (txOutValue victimDepositOut)
    assertWalletLovelace opts leaderDepositorVk 0
    assertWalletLovelace opts victimVk 0
    assertWalletLovelace opts lootVk 0
