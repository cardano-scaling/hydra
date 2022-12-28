-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'CommitMutation'.
module Hydra.Chain.Direct.Contract.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

-- Arbitrary VerificationKey instance
import Hydra.Chain.Direct.TxSpec ()

import qualified Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  replacePolicyIdWith,
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (commitTx, headPolicyId, mkHeadId, mkInitialOutput)
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (
  genAddressInEra,
  genOutput,
  genValue,
  genVerificationKey,
 )
import Hydra.Party (Party)
import Test.QuickCheck (oneof, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (Tx, UTxO)
healthyCommitTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (Fixture.testSeedInput, toUTxOContext initialOutput)
      <> UTxO.singleton healthyCommittedUTxO
      <> registryUTxO scriptRegistry
  tx =
    commitTx
      scriptRegistry
      Fixture.testNetworkId
      (mkHeadId Fixture.testPolicyId)
      commitParty
      (Just healthyCommittedUTxO)
      (Fixture.testSeedInput, toUTxOContext initialOutput, initialPubKeyHash)

  scriptRegistry = genScriptRegistry `generateWith` 42

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  commitParty :: Party
  commitParty = generateWith arbitrary 42

commitVerificationKey :: VerificationKey PaymentKey
commitVerificationKey = generateWith arbitrary 42

initialOutput :: TxOut CtxTx
initialOutput = mkInitialOutput Fixture.testNetworkId Fixture.testPolicyId commitVerificationKey

-- NOTE: An 8₳ output which is currently addressed to some arbitrary key.
healthyCommittedUTxO :: (TxIn, TxOut CtxUTxO)
healthyCommittedUTxO = flip generateWith 42 $ do
  txIn <- arbitrary
  txOut <- modifyTxOutValue (const $ lovelaceToValue 8_000_000) <$> (genOutput =<< arbitrary)
  pure (txIn, txOut)

data CommitMutation
  = MutateCommitOutputValue
  | MutateCommittedValue
  | MutateCommittedAddress
  | MutateRequiredSigner
  | -- | Change the policy Id of the PT both in input and output
    MutatePolicyId
  deriving (Generic, Show, Enum, Bounded)

genCommitMutation :: (Tx, UTxO) -> Gen SomeMutation
genCommitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateCommitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- genValue `suchThat` (/= commitOutputValue)
        pure $ commitTxOut{txOutValue = mutatedValue}
    , SomeMutation MutateCommittedValue <$> do
        mutatedValue <- genValue `suchThat` (/= committedOutputValue)
        let mutatedOutput = modifyTxOutValue (const mutatedValue) committedTxOut

        pure $ ChangeInput committedTxIn mutatedOutput Nothing
    , SomeMutation MutateCommittedAddress <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId `suchThat` (/= committedAddress)
        let mutatedOutput = modifyTxOutAddress (const mutatedAddress) committedTxOut
        pure $ ChangeInput committedTxIn mutatedOutput Nothing
    , SomeMutation MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation MutatePolicyId <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= Fixture.testSeedInput))
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId commitTxOut)
            , ChangeInput
                Fixture.testSeedInput
                (toUTxOContext $ replacePolicyIdWith Fixture.testPolicyId otherHeadId initialOutput)
                (Just $ toScriptData $ Initial.ViaCommit $ Just $ toPlutusTxOutRef committedTxIn)
            ]
    ]
 where
  TxOut{txOutValue = commitOutputValue} = commitTxOut

  commitTxOut = fromJust $ txOuts' tx !!? 0

  (committedTxIn, committedTxOut) = healthyCommittedUTxO

  committedAddress = txOutAddress committedTxOut

  committedOutputValue = txOutValue committedTxOut
