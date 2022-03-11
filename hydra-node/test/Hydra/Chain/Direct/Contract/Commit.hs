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
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (commitTx, headPolicyId, mkInitialOutput)
import Hydra.Ledger.Cardano (
  genAddressInEra,
  genOutput,
  genValue,
  genVerificationKey,
 )
import Test.QuickCheck (oneof, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (Tx, UTxO)
healthyCommitTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (initialInput, toUTxOContext initialOutput)
      <> UTxO.singleton healthyCommittedUTxO
  tx =
    commitTx
      Fixture.testNetworkId
      (Just healthyCommittedUTxO)
      (initialInput, toUTxOContext initialOutput, initialPubKeyHash)

  initialInput = generateWith arbitrary 42

  initialOutput = mkInitialOutput Fixture.testNetworkId policyId commitVerificationKey

  policyId = headPolicyId initialInput

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  commitVerificationKey :: VerificationKey PaymentKey
  commitVerificationKey = generateWith arbitrary 42

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
  deriving (Generic, Show, Enum, Bounded)

genCommitMutation :: (Tx, UTxO) -> Gen SomeMutation
genCommitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateCommitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- genValue `suchThat` (/= commitOutputValue)
        pure $ TxOut commitOutputAddress mutatedValue commitOutputDatum
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
    ]
 where
  TxOut commitOutputAddress commitOutputValue commitOutputDatum =
    fromJust $ txOuts' tx !!? 0

  (committedTxIn, committedTxOut) = healthyCommittedUTxO

  committedAddress = txOutAddress committedTxOut

  committedOutputValue = txOutValue committedTxOut
