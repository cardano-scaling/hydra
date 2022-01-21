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
  lookupUtxo = singletonUtxo (initialInput, toUtxoContext initialOutput)

  tx =
    commitTx
      Fixture.testNetworkId
      healthyCommitParty
      healthyCommittedUtxo
      (initialInput, initialPubKeyHash)

  initialInput = generateWith arbitrary 42

  initialOutput = mkInitialOutput Fixture.testNetworkId healthyCommitVerificationKey

  initialPubKeyHash = generateWith arbitrary 42

  -- NOTE: An ada-only output which is currently addressed to some arbitrary
  -- public key.
  healthyCommittedUtxo :: Maybe (TxIn, TxOut CtxUTxO Era)
  healthyCommittedUtxo = flip generateWith 42 $ do
    txIn <- arbitrary
    txOut <- adaOnly <$> (genOutput =<< arbitrary)
    pure $ Just (txIn, txOut)

  healthyCommitVerificationKey :: VerificationKey PaymentKey
  healthyCommitVerificationKey = generateWith arbitrary 42

  healthyCommitParty :: Party
  healthyCommitParty = generateWith arbitrary 42
