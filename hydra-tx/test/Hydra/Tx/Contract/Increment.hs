{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Increment where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-plutus" Hydra.Plutus.Gen ()
import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (arbitrarySizedNatural, elements, oneof, suchThat)
import "base" Data.Maybe (fromJust)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-cardano-api" Hydra.Cardano.Api.Gen (genTxIn)
import "hydra-plutus" Hydra.Contract.Commit (Commit)
import "hydra-plutus" Hydra.Contract.Deposit (DepositRedeemer (Claim))
import "hydra-plutus" Hydra.Contract.DepositError (DepositError (..))
import "hydra-plutus" Hydra.Contract.Error (toErrorCode)
import "hydra-plutus" Hydra.Contract.HeadError (HeadError (..))
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Data.Party qualified as OnChain
import "hydra-plutus-extras" Hydra.Plutus.Orphans ()

import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Contract.Deposit (healthyDeadline)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign, toPlutusSignatures)
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.Deposit qualified as Deposit
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (adaOnly)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genForParty, genScriptRegistry, genUTxOSized, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotVersion,
 )
import "plutus-ledger-api" PlutusLedgerApi.V2 qualified as Plutus
import "plutus-tx" PlutusTx.Builtins (toBuiltin)
import "quickcheck-instances" Test.QuickCheck.Instances ()

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
      (mkHeadId testPolicyId)
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
      }

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput =
    mkHeadOutput testNetworkId testPolicyId (mkTxOutDatumInline healthyDatum)
      & addParticipationTokens healthyParticipants
      & modifyTxOutValue (<> UTxO.totalValue healthyUTxO)

  depositUTxO :: UTxO
  depositUTxO =
    UTxO.singleton healthyDepositInput $
      mkDepositOutput testNetworkId (mkHeadId testPolicyId) healthyDeposited healthyDeadline

healthyDepositInput :: TxIn
healthyDepositInput = arbitrary `generateWith` 123

healthyDeposited :: UTxO
healthyDeposited = genUTxOSized 3 `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` 42

healthySigningKeys :: [SigningKey HydraKey]
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
    }

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = UTxO.map adaOnly $ generateWith (genUTxOSized 3) 42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    Head.OpenDatum
      { utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , parties = healthyOnChainParties
      , contestationPeriod = toChain healthyContestationPeriod
      , headId = toPlutusCurrencySymbol testPolicyId
      , version = toInteger healthySnapshotVersion
      }

data IncrementMutation
  = -- | Move the deadline from the deposit datum back in time
    -- so that the increment upper bound is after the deadline
    DepositMutateDepositPeriod
  | -- | Alter the head id
    DepositMutateHeadId
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
  | -- | Alter the Claim redeemer `TxOutRef`
    IncrementDifferentClaimRedeemer
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
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Claim (toPlutusCurrencySymbol testPolicyId))
    , SomeMutation (pure $ toErrorCode WrongHeadIdInDepositDatum) DepositMutateHeadId <$> do
        otherHeadId <- arbitrary
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((_headCS, depositDatumDeadline, commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (otherHeadId, depositDatumDeadline, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Claim (toPlutusCurrencySymbol testPolicyId))
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
              }
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  (depositIn, depositOut@(TxOut addr val _ rscript)) =
    fromJust $
      find
        (\(_, TxOut address _ _ _) -> address == Deposit.depositAddress testNetworkId)
        (UTxO.toList (resolveInputsUTxO utxo tx))
