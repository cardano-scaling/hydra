-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'CommitMutation'.
module Hydra.Chain.Direct.Contract.Commit where

import Hydra.Prelude

-- Arbitrary VerificationKey instance
import Hydra.Chain.Direct.TxSpec ()

import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  isInitialOutput,
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (commitTx, mkInitialOutput)
import Hydra.Ledger.Cardano (
  CardanoTx,
  CtxUTxO,
  Era,
  PaymentKey,
  TxIn,
  TxOut (TxOut),
  Utxo,
  VerificationKey,
  genOutput,
  genValue,
  getOutputs,
  lovelaceToValue,
  mkTxOutValue,
  modifyTxOutValue,
  singletonUtxo,
  toUtxoContext,
  utxoPairs,
  verificationKeyHash,
 )
import Hydra.Party (Party)
import Test.QuickCheck (elements, oneof, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (CardanoTx, Utxo)
healthyCommitTx =
  (tx, lookupUtxo)
 where
  lookupUtxo =
    singletonUtxo (initialInput, toUtxoContext initialOutput)
      <> singletonUtxo committedUtxo

  tx =
    commitTx
      Fixture.testNetworkId
      commitParty
      (Just committedUtxo)
      (initialInput, initialPubKeyHash)

  initialInput = generateWith arbitrary 42

  initialOutput = mkInitialOutput Fixture.testNetworkId commitVerificationKey

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  -- NOTE: An 8₳ output which is currently addressed to some arbitrary key.
  committedUtxo :: (TxIn, TxOut CtxUTxO Era)
  committedUtxo = flip generateWith 42 $ do
    txIn <- arbitrary
    txOut <- modifyTxOutValue (const $ lovelaceToValue 8_000_000) <$> (genOutput =<< arbitrary)
    pure (txIn, txOut)

  commitVerificationKey :: VerificationKey PaymentKey
  commitVerificationKey = generateWith arbitrary 42

  commitParty :: Party
  commitParty = generateWith arbitrary 42

data CommitMutation
  = MutateCommitOutputValue
  | MutateComittedValue
  deriving (Generic, Show, Enum, Bounded)

genCommitMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genCommitMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateCommitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- (mkTxOutValue <$> genValue) `suchThat` (/= commitOutputValue)
        pure $ TxOut commitOutputAddress mutatedValue commitOutputDatum
    , SomeMutation MutateComittedValue <$> do
        (comittedTxIn, _) <- elements comittedTxIns
        newResolvedTxIn <- genOutput =<< arbitrary
        pure $ ChangeInput comittedTxIn newResolvedTxIn
    ]
 where
  TxOut commitOutputAddress commitOutputValue commitOutputDatum =
    fromJust $ getOutputs tx !!? 0

  -- NOTE: This filtering will also yield any input added for fees, but we don't
  -- have any in our test scenario so far.
  comittedTxIns =
    filter (not . isInitialOutput . snd) . utxoPairs $ utxo
