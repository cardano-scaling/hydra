-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'genCommitMutation'
module Hydra.Chain.Direct.Contract.Commit where

import Hydra.Prelude

import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (commitTx, mkInitialOutput)
import Hydra.Ledger.Cardano (
  CardanoTx,
  CtxUTxO,
  Era,
  PaymentKey,
  TxIn,
  TxOut,
  Utxo,
  VerificationKey,
  adaOnly,
  genOutput,
  singletonUtxo,
  toUtxoContext,
  verificationKeyHash,
 )
import Hydra.Party (Party)

-- Arbitrary VerificationKey instance
import Hydra.Chain.Direct.TxSpec ()

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

  -- NOTE: An ada-only output which is currently addressed to some arbitrary
  -- public key.
  committedUtxo :: (TxIn, TxOut CtxUTxO Era)
  committedUtxo = flip generateWith 42 $ do
    txIn <- arbitrary
    txOut <- adaOnly <$> (genOutput =<< arbitrary)
    pure (txIn, txOut)

  commitVerificationKey :: VerificationKey PaymentKey
  commitVerificationKey = generateWith arbitrary 42

  commitParty :: Party
  commitParty = generateWith arbitrary 42
