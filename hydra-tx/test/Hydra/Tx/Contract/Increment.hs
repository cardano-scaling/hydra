{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Increment where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.Commit (Commit, serializeCommit)
import Hydra.Contract.Deposit (DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.UtilError (UtilError (MintingOrBurningIsForbidden))
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Contract.Deposit (healthyDeadline)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign, toPlutusSignatures)
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.Deposit qualified as Deposit
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Secret (Secret)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, dperiod, slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genForParty, genMintedOrBurnedValue, genScriptRegistry, genUTxOSized, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyIncrementTx :: (Tx, UTxO)
healthyIncrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> depositUTxO
      <> registryUTxO scriptRegistry

  tx =
    incrementTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (testSeedInput, mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      depositUTxO
      (slotNoFromUTCTime systemStart slotLength healthyDeadline)
      healthySignature

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      , depositPeriod = dperiod
      }

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput =
    mkHeadOutput @CtxUTxO
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline healthyDatum)
      & modifyTxOutValue (<> UTxO.totalValue healthyUTxO)

  depositUTxO :: UTxO
  depositUTxO =
    UTxO.singleton healthyDepositInput $
      mkDepositOutput testNetworkId (mkHeadId testPolicyId) healthyDeposited healthyDeadline

-- | The deposit is the first output of its transaction, as built by
-- 'Hydra.Tx.Deposit.depositTx' and required by 'checkIncrement'.
healthyDepositInput :: TxIn
healthyDepositInput = TxIn healthyDepositTxId (TxIx 0)

healthyDepositTxId :: TxId
healthyDepositTxId = arbitrary `generateWith` 123

healthyDeposited :: UTxO
healthyDeposited = genUTxOSized 3 `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` 42

healthySigningKeys :: [Secret (SigningKey HydraKey)]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: MultiSignature (Snapshot Tx)
healthySignature = aggregate [sign sk healthySnapshot | sk <- healthySigningKeys]

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthySnapshotVersion :: SnapshotVersion
healthySnapshotVersion = 1

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , version = healthySnapshotVersion
    , number = succ healthySnapshotNumber
    , confirmed = []
    , utxo = healthyUTxO
    , utxoToCommit = Just healthyDeposited
    , utxoToDecommit = Nothing
    , depositTxId = Just healthyDepositTxId
    , accumulator = healthyAccumulator
    }

healthyAccumulatorHash :: ByteString
healthyAccumulatorHash = Accumulator.getAccumulatorHash healthyAccumulator

healthyAccumulator :: Accumulator.HydraAccumulator
healthyAccumulator = Accumulator.buildFromSnapshotUTxOs healthyUTxO (Just healthyDeposited) Nothing

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = UTxO.map adaOnly $ generateWith (genUTxOSized 3) 42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , contestationPeriod = toChain healthyContestationPeriod
      , depositPeriod = DP.toChain dperiod
      , headSeed = toPlutusTxOutRef testSeedInput
      , headId = toPlutusCurrencySymbol testPolicyId
      , version = toInteger healthySnapshotVersion
      , accumulatorHash = toBuiltin healthyAccumulatorHash
      , headAdaOverhead = 0
      }

data IncrementMutation
  = -- | Move the deadline from the deposit datum back in time
    -- so that the increment upper bound is after the deadline
    DepositMutateDepositPeriod
  | -- | Change the head id stored in the deposit datum away from the
    -- head being incremented; checkIncrement must reject this.
    DepositMutateHeadId
  | -- | SECURITY: redirect the committed outputs recorded in the claimed
    -- deposit's datum to an attacker address, preserving each output's value
    -- (and the deposit UTxO's own value). Only the committed identity changes,
    -- so the recomputed commit hash no longer matches the signed snapshot and
    -- signature verification must fail.
    RedirectCommitOutput
  | -- | Change parties in increment output datum
    IncrementMutateParties
  | -- | New version is incremented correctly
    IncrementUseDifferentSnapshotVersion
  | -- | Produce invalid signatures
    ProduceInvalidSignatures
  | -- | Change the head value
    ChangeHeadValue
  | -- | Change the required signers
    AlterRequiredSigner
  | -- | Alter the Claim redeemer `TxOutRef` to a deposit input not spent by the
    -- tx. 'checkIncrement' recomputes the committed-outputs hash from the CLAIMED
    -- deposit input, so a missing claim ref hard-fails with 'DepositInputNotFound'
    -- (this now precedes and subsumes the 'DepositNotSpent' check).
    IncrementDifferentClaimRedeemer
  | -- | SECURITY: claim a look-alike deposit instead of the approved one. The
    -- substitute is created by a different transaction but carries a
    -- byte-identical datum and the same value, so it hashes the same and
    -- preserves the head value; only the deposit's identity differs. The signed
    -- snapshot binds that identity, so signature verification must fail.
    IncrementClaimLookAlikeDeposit
  | -- | SECURITY: claim a sibling output of the approved deposit's own
    -- transaction, carrying a copied datum. The signed message binds the deposit
    -- by transaction id, which the sibling shares, so the signature still
    -- verifies and only the output index tells the two apart.
    IncrementClaimSiblingDepositOutput
  | -- | Add a second v_deposit input alongside an attacker-controlled
    -- output that redirects its value away from the head's continuation.
    IncrementAddExtraDepositInput
  | -- | Minting or burning of tokens should not be possible in increment.
    MutateTokenMintingOrBurning
  deriving stock (Generic, Show, Enum, Bounded)

genIncrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genIncrementMutation (tx, utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode DepositPeriodSurpassed) DepositMutateDepositPeriod <$> do
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((headCS', depositDatumDeadline, commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (headCS', Plutus.POSIXTime $ Plutus.getPOSIXTime depositDatumDeadline - 1000, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData Claim)
    , SomeMutation (pure $ toErrorCode DepositHeadInputNotFound) DepositMutateHeadId <$> do
        otherHeadId <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((_headCS, depositDatumDeadline, commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (otherHeadId, depositDatumDeadline, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData Claim)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) RedirectCommitOutput <$> do
        attackerVk <- genVerificationKey
        let attackerAddr = mkVkAddress testNetworkId attackerVk
            -- Redirect every committed output to the attacker, preserving each
            -- output's value; keep the deposit UTxO's own addr/val/rscript so the
            -- head value check is unaffected and only the committed identity moves.
            mutatedCommits =
              mapMaybe
                (\(i, o) -> serializeCommit (i, modifyTxOutAddress (const attackerAddr) o))
                (UTxO.toList healthyDeposited)
            datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((headCS', deadline, _commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (headCS', deadline, mutatedCommits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData Claim)
    , SomeMutation (pure $ toErrorCode ChangedParameters) IncrementMutateParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode VersionNotIncremented) IncrementUseDifferentSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthySnapshotVersion + 1)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ProduceInvalidSignatures . ChangeHeadRedeemer <$> do
        invalidSignature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $
          Head.Increment
            Head.IncrementRedeemer
              { signature =
                  invalidSignature
              , snapshotNumber = fromIntegral healthySnapshotNumber
              , increment = toPlutusTxOutRef healthyDepositInput
              , decommitOutputsHash = toBuiltin $ hashUTxO @Tx (mempty :: UTxO)
              }
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ChangeHeadValue <$> do
        newValue <- genValue `suchThat` (/= txOutValue headTxOut)
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) AlterRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode DepositInputNotFound) IncrementDifferentClaimRedeemer . ChangeHeadRedeemer <$> do
        invalidDepositRef <- genTxIn
        pure $
          Head.Increment
            Head.IncrementRedeemer
              { signature = toPlutusSignatures healthySignature
              , snapshotNumber = fromIntegral $ succ healthySnapshotNumber
              , increment = toPlutusTxOutRef invalidDepositRef
              , decommitOutputsHash = toBuiltin $ hashUTxO @Tx (mempty :: UTxO)
              }
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) IncrementClaimLookAlikeDeposit <$> do
        lookAlikeIn <- genTxIn `suchThat` (\(TxIn tid _) -> tid /= healthyDepositTxId)
        pure $
          Changes
            [ RemoveInput depositIn
            , AddInput lookAlikeIn depositOut (Just $ toScriptData Claim)
            , ChangeHeadRedeemer $
                Head.Increment
                  Head.IncrementRedeemer
                    { signature = toPlutusSignatures healthySignature
                    , snapshotNumber = fromIntegral $ succ healthySnapshotNumber
                    , increment = toPlutusTxOutRef lookAlikeIn
                    , decommitOutputsHash = toBuiltin $ hashUTxO @Tx (mempty :: UTxO)
                    }
            ]
    , SomeMutation (pure $ toErrorCode DepositNotFirstOutput) IncrementClaimSiblingDepositOutput <$> do
        let siblingIn = TxIn healthyDepositTxId (TxIx 1)
        pure $
          Changes
            [ RemoveInput depositIn
            , AddInput siblingIn depositOut (Just $ toScriptData Claim)
            , ChangeHeadRedeemer $
                Head.Increment
                  Head.IncrementRedeemer
                    { signature = toPlutusSignatures healthySignature
                    , snapshotNumber = fromIntegral $ succ healthySnapshotNumber
                    , increment = toPlutusTxOutRef siblingIn
                    , decommitOutputsHash = toBuiltin $ hashUTxO @Tx (mempty :: UTxO)
                    }
            ]
    , SomeMutation [toErrorCode DepositNotClaimedByHead, toErrorCode HeadValueIsNotPreserved] IncrementAddExtraDepositInput <$> do
        extraIn <- genTxIn `suchThat` (/= depositIn)
        extraDeposited <- UTxO.map adaOnly <$> genUTxOSized 1
        attackerVk <- genVerificationKey
        let extraDepositOut :: TxOut CtxUTxO
            extraDepositOut =
              mkDepositOutput testNetworkId (mkHeadId testPolicyId) extraDeposited healthyDeadline
            attackerOut :: TxOut CtxTx
            attackerOut =
              TxOut
                (mkVkAddress testNetworkId attackerVk)
                (txOutValue extraDepositOut)
                TxOutDatumNone
                ReferenceScriptNone
        pure $
          Changes
            [ AddInput extraIn extraDepositOut (Just $ toScriptData Claim)
            , AppendOutput attackerOut
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  (depositIn, depositOut@(TxOut addr val _ rscript)) =
    fromJust $
      find
        (\(_, TxOut address _ _ _) -> address == Deposit.depositAddress testNetworkId)
        (UTxO.toList (resolveInputsUTxO utxo tx))
