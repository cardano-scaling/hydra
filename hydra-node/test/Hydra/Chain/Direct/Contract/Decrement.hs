{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.Contract.Decrement where

import Hydra.Cardano.Api
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotNumberInOpen,
 )
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  decrementTx,
  mkHeadId,
  mkHeadOutput,
 )
import Hydra.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (HydraKey, MultiSignature (..), aggregate, sign, toPlutusSignatures)
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger (IsTx (hashUTxO, withoutUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genUTxOSized,
  genValue,
  genVerificationKey,
 )
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk, genForParty)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, oneof)
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()

healthyDecrementTx :: (Tx, UTxO)
healthyDecrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headOutput)
      <> registryUTxO scriptRegistry

  tx =
    decrementTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      healthySignature

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

healthySnapshot :: Snapshot Tx
healthySnapshot =
  let (utxoToDecommit', utxo) = splitDecommitUTxO healthyUTxO
   in Snapshot
        { headId = mkHeadId testPolicyId
        , number = succ healthySnapshotNumber
        , utxo
        , confirmed = []
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
        { utxoHash = toBuiltin $ hashUTxO @Tx utxo
        , parties = healthyOnChainParties
        , contestationPeriod = toChain healthyContestationPeriod
        , snapshotNumber = toInteger healthySnapshotNumber
        , headId = toPlutusCurrencySymbol testPolicyId
        }

data DecrementMutation
  = -- | Ensures parties do not change between head input datum and head output
    --  datum.
    MutatePartiesInOutput
  | -- | Invalidates the tx by changing the snapshot number in resulting head
    -- output.
    MutateSnapshotNumber
  | -- | Produce invalid signature by changing signers in the redeemer
    SnapshotSignatureInvalid
  | -- | Ensures decrement is authenticated by one of the Head members by changing
    --  the signer used on the tx to not be one of PTs.
    MutateRequiredSigner
  | -- | Mutate the output value to produce different 'UTxO' hash to the one in the signed 'Snapshot'.
    MutateChangeOutputValue
  | -- | Invalidates the tx by changing the output values arbitrarily to be
    -- different (not preserved) from the head.
    --
    -- Ensures values are preserved between head input and output.
    MutateValueInOutput
  | -- | Drop one of the decommit outputs from the tx. This should trigger snapshot signature validation to fail.
    DropDecommitOutput
  deriving stock (Generic, Show, Enum, Bounded)

genDecrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genDecrementMutation (tx, utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (Just $ toErrorCode SnapshotNumberMismatch) MutateSnapshotNumber <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (< healthySnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumberInOpen $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) SnapshotSignatureInvalid . ChangeHeadRedeemer <$> do
        Head.Decrement . toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx))) <*> pure (fromIntegral $ length utxo - 1)
    , SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) MutateChangeOutputValue <$> do
        let outs = txOuts' tx
        -- NOTE: Skip the first output since this is the Head output.
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (Just $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) DropDecommitOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
