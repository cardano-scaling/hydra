{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Decrement where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotVersion,
 )

import Cardano.Api.Shelley (SigningKey, VerificationKey, verificationKeyHash)
import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.Party qualified as OnChain
import Hydra.Plutus.Orphans ()
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Contract.CollectCom (extractHeadOutputValue)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign, toPlutusSignatures)
import Hydra.Tx.Decrement (
  decrementTx,
 )
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO, withoutUTxO))
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (adaOnly, splitUTxO)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genForParty, genScriptRegistry, genUTxOSized, genValue, genVerificationKey)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, oneof)
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()

healthyDecrementTx :: (Tx, UTxO)
healthyDecrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
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
    mkHeadOutput testNetworkId testPolicyId (mkTxOutDatumInline healthyDatum)
      & addParticipationTokens healthyParticipants
      & modifyTxOutValue (<> UTxO.totalValue healthyUTxO)

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
  case UTxO.toList utxo of
    [] -> error "empty utxo in splitDecommitUTxO"
    (decommit : _rest) ->
      let decommitUTxO' = UTxO.fromList [decommit]
       in (utxo `withoutUTxO` decommitUTxO', decommitUTxO')

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = UTxO.map adaOnly $ generateWith (genUTxOSized 3) 42

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

data DecrementMutation
  = -- | Ensures parties do not change between head input datum and head output
    --  datum.
    ChangePartiesInOutput
  | -- | Produce invalid signature by changing signers in the redeemer
    ProduceInvalidSignatures
  | -- | Ensures decrement is authenticated by one of the Head members by changing
    --  the signer used on the tx to not be one of PTs.
    AlterRequiredSigner
  | -- | Mutate the output value to produce different 'UTxO' hash to the one in the signed 'Snapshot'.
    ChangeDecrementedValue
  | -- | Invalidates the tx by changing the output values arbitrarily to be
    -- different (not preserved) from the head.
    --
    -- Ensures values are preserved between head input and output.
    ChangeHeadValue
  | -- | Drop one of the decommit outputs from the tx. This should trigger snapshot signature validation to fail.
    DropDecommitOutput
  | ExtractSomeValue
  | -- | Invalidates the tx by changing the snapshot version in resulting head
    -- output.
    UseDifferentSnapshotVersion
  deriving stock (Generic, Show, Enum, Bounded)

genDecrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genDecrementMutation (tx, _utxo) =
  oneof
    [ -- Spec: parameters cid, ̃kH,n,T stay unchanged
      SomeMutation (pure $ toErrorCode ChangedParameters) ChangePartiesInOutput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , -- New version v′ is incremented correctly
      SomeMutation (pure $ toErrorCode VersionNotIncremented) UseDifferentSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthySnapshotVersion + 1)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , -- Spec: ξ is a valid multi-signature of the currency id cid, the current
      -- snapshot state η, the new snapshot number s′ and state η
      SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ProduceInvalidSignatures . ChangeHeadRedeemer <$> do
        invalidSignature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $
          Head.Decrement
            Head.DecrementRedeemer
              { signature = invalidSignature
              , snapshotNumber = fromIntegral healthySnapshotNumber
              , numberOfDecommitOutputs = fromIntegral $ maybe 0 UTxO.size $ utxoToDecommit healthySnapshot
              }
    , -- Spec: Transaction is signed by a participant
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) AlterRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ChangeDecrementedValue <$> do
        let outs = txOuts' tx
        -- NOTE: Skip the first output since this is the Head output.
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- Spec: The value in the head output is decreased accordingly
      SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ChangeHeadValue <$> do
        newValue <- genValue `suchThat` (/= txOutValue headTxOut)
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) DropDecommitOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ExtractSomeValue <$> do
        extractHeadOutputValue headTxOut testPolicyId
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
