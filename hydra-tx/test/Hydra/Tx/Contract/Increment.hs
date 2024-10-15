{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Increment where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Tx.Mutation (
  Mutation (ChangeInput, ChangeOutput),
  SomeMutation (..),
  addParticipationTokens,
  modifyInlineDatum,
  replaceParties,
 )

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Contract.Deposit (DepositDatum (..), DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Contract.Deposit (depositDeadline, healthyDepositTx)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign)
import Hydra.Tx.HeadId (headIdToCurrencySymbol, mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Increment (
  incrementTx,
 )
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO, withoutUTxO))
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (adaOnly, splitUTxO)
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, slotLength, systemStart, testHeadId, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genForParty, genScriptRegistry, genUTxOSized, genVerificationKey)
import Test.QuickCheck (elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyIncrementTx :: (Tx, UTxO)
healthyIncrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headOutput)
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
      (slotNoFromUTCTime systemStart slotLength depositDeadline)
      healthySignature

  depositUTxO = utxoFromTx $ fst healthyDepositTx

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput =
    mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatumInline healthyDatum)
      & addParticipationTokens healthyParticipants
      & modifyTxOutValue (<> foldMap txOutValue healthyUTxO)

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
  let (utxoToDecommit', utxo) = splitUTxO healthyUTxO
   in Snapshot
        { headId = mkHeadId testPolicyId
        , version = healthySnapshotVersion
        , number = succ healthySnapshotNumber
        , confirmed = []
        , utxo
        , utxoToCommit = Nothing
        , utxoToDecommit = Just utxoToDecommit'
        }

splitDecommitUTxO :: UTxO -> (UTxO, UTxO)
splitDecommitUTxO utxo =
  case UTxO.pairs utxo of
    [] -> error "empty utxo in splitDecommitUTxO"
    (decommit : _rest) ->
      let decommitUTxO' = UTxO.fromPairs [decommit]
       in (utxo `withoutUTxO` decommitUTxO', decommitUTxO')

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = adaOnly <$> generateWith (genUTxOSized 3) 42

healthyDatum :: Head.State
healthyDatum =
  let (_utxoToDecommit', utxo) = splitDecommitUTxO healthyUTxO
   in Head.Open
        Head.OpenDatum
          { utxoHash = toBuiltin $ hashUTxO @Tx utxo
          , parties = healthyOnChainParties
          , contestationPeriod = toChain healthyContestationPeriod
          , headId = toPlutusCurrencySymbol testPolicyId
          , version = toInteger healthySnapshotVersion
          }

data IncrementMutation
  = -- | Move the deadline from the deposit datum back in time
    -- so that the increment upper bound is after the deadline
    DepositMutateDepositDeadline
  | -- | Alter the head id
    DepositMutateHeadId
  | -- | Change parties in incrment output datum
    IncrementMutateParties
  deriving stock (Generic, Show, Enum, Bounded)

genIncrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genIncrementMutation (tx, utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode DepositDeadlineSurpassed) DepositMutateDepositDeadline <$> do
        let (depositIn, depositOut@(TxOut addr val _ rscript)) = UTxO.pairs (resolveInputsUTxO utxo tx) List.!! 1
        let datum =
              txOutDatum $
                flip modifyInlineDatum (toTxContext depositOut) $ \case
                  DepositDatum (headCS', depositDatumDeadline, commits) ->
                    DepositDatum (headCS', Plutus.POSIXTime $ Plutus.getPOSIXTime depositDatumDeadline - 1, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Claim (headIdToCurrencySymbol testHeadId))
    , SomeMutation (pure $ toErrorCode WrongHeadIdInDepositDatum) DepositMutateHeadId <$> do
        otherHeadId <- arbitrary
        let (depositIn, depositOut@(TxOut addr val _ rscript)) = UTxO.pairs (resolveInputsUTxO utxo tx) List.!! 1
        let datum =
              txOutDatum $
                flip modifyInlineDatum (toTxContext depositOut) $ \case
                  DepositDatum (_headCS, depositDatumDeadline, commits) ->
                    DepositDatum (otherHeadId, depositDatumDeadline, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Claim (headIdToCurrencySymbol testHeadId))
    , SomeMutation (pure $ toErrorCode ChangedParameters) IncrementMutateParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
