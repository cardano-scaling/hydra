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

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.CollectCom (extractHeadOutputValue)
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (splitUTxO)
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
import Hydra.Ledger.Cardano (adaOnly, genUTxOSized, genValue, genVerificationKey)
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

healthySnapshot :: Snapshot Tx
healthySnapshot =
  let (utxoToDecommit', utxo) = splitUTxO healthyUTxO
   in Snapshot
        { headId = mkHeadId testPolicyId
        , number = succ healthySnapshotNumber
        , utxo
        , confirmed = []
        , utxoToDecommit = Just utxoToDecommit'
        , version = toInteger healthySnapshotNumber
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
        , version = 1
        }

data DecrementMutation
  = -- | Ensures parties do not change between head input datum and head output
    --  datum.
    ChangePartiesInOuput
  | -- | Invalidates the tx by changing the snapshot number in resulting head
    -- output.
    UseDifferentSnapshotNumber
  | -- | Produce invalid signature by changing signers in the redeemer
    ProduceInvalidSignatures
  | -- | Ensures decrement is authenticated by one of the Head members by changing
    --  the signer used on the tx to not be one of PTs.
    AlterRequiredSigner
  | -- | Mutate the output value to produce different 'UTxO' hash to the one in the signed 'Snapshot'.
    ChangeOutputValue
  | -- | Invalidates the tx by changing the output values arbitrarily to be
    -- different (not preserved) from the head.
    --
    -- Ensures values are preserved between head input and output.
    ChangeValueInOutput
  | -- | Drop one of the decommit outputs from the tx. This should trigger snapshot signature validation to fail.
    DropDecommitOutput
  | ExtractSomeValue
  deriving stock (Generic, Show, Enum, Bounded)

genDecrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genDecrementMutation (tx, utxo) =
  oneof
    [ -- XXX: parameters cid, ̃kH,n,T stay unchanged
      SomeMutation (pure $ toErrorCode ChangedParameters) ChangePartiesInOuput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , -- XXX: Decrement snapshot number s′ is higher than the currently stored snapshot number s
      SomeMutation (pure $ toErrorCode SnapshotNumberMismatch) UseDifferentSnapshotNumber <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (< healthySnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumberInOpen $ toInteger mutatedSnapshotNumber) headTxOut
    , -- XXX: ξ is a valid multi-signature of the currency id cid, the current snapshot state η,
      -- the new snapshot number s′ and state η
      SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ProduceInvalidSignatures . ChangeHeadRedeemer <$> do
        Head.Decrement . toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx))) <*> pure (fromIntegral $ length utxo - 1)
    , -- XXX: Transaction is signed by a participant
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) AlterRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ChangeOutputValue <$> do
        let outs = txOuts' tx
        -- NOTE: Skip the first output since this is the Head output.
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- XXX: The value in the head output is decreased accordingly
      SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ChangeValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) DropDecommitOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ExtractSomeValue <$> do
        extractHeadOutputValue headTxOut testPolicyId
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
