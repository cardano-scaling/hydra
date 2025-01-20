-- | Utilities to building transactions on top of the cardano-api.
module Hydra.Ledger.Cardano.Builder where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.Map qualified as Map

-- * Executing

-- | Construct a transction from a builder. It is said 'unsafe' because the
-- underlying implementation will perform some sanity check on a transaction;
-- for example, check that it has at least one input, that no outputs are
-- negatives and whatnot.
--
-- We use the builder only internally for on-chain transaction crafted in the
-- context of Hydra.
unsafeBuildTransaction :: HasCallStack => TxBodyContent BuildTx -> Tx
unsafeBuildTransaction builder =
  either
    (\txBodyError -> bug $ InvalidTransactionException{txBodyError, builder})
    (`Tx` mempty)
    . createAndValidateTransactionBody
    $ builder

-- | A runtime exception to capture (programmer) failures when building
-- transactions. This should never happened in practice (famous last words...)!
data InvalidTransactionException = InvalidTransactionException
  { txBodyError :: TxBodyError
  , builder :: TxBodyContent BuildTx
  }
  deriving stock (Show)

instance Exception InvalidTransactionException

-- | Like 'addInputs' but only for vk inputs which requires no additional data.
addTxInsSpending :: [TxIn] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addTxInsSpending ins =
  addTxIns ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

addVkInputs :: [TxIn] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addVkInputs ins =
  addTxIns ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

addCollateralInput :: TxIn -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addCollateralInput txin tx =
  tx{txInsCollateral = TxInsCollateral [txin]}

changePParams :: PParams (ShelleyLedgerEra Era) -> TxBodyContent BuildTx -> TxBodyContent BuildTx
changePParams pparams = setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

-- | Append new outputs to an ongoing builder.
addOutputs :: [TxOut CtxTx] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addOutputs outputs tx =
  tx{txOuts = txOuts tx <> outputs}

-- | Add extra required key witnesses to a transaction.
addExtraRequiredSigners :: [Hash PaymentKey] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addExtraRequiredSigners vks tx =
  tx{txExtraKeyWits = txExtraKeyWits'}
 where
  txExtraKeyWits' =
    case txExtraKeyWits tx of
      TxExtraKeyWitnessesNone ->
        TxExtraKeyWitnesses vks
      TxExtraKeyWitnesses vks' ->
        TxExtraKeyWitnesses (vks' <> vks)

addCertificates :: TxCertificates BuildTx Era -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addCertificates certs tx =
  tx{txCertificates = certs}

-- | Mint tokens with given plutus minting script and redeemer.
mintTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
mintTokens script redeemer assets = addTxMintValue newTokens
 where
  newTokens =
    Map.fromList [(policyId, fmap (\(x, y) -> (x, y, BuildTxWith mintingWitness)) assets)]

  mintingWitness =
    mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)

  policyId =
    PolicyId $ hashScript $ PlutusScript script

-- | Burn tokens with given plutus minting script and redeemer.
-- This is really just `mintTokens` with negated 'Quantity'.
burnTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
burnTokens script redeemer assets =
  mintTokens script redeemer (fmap (second negate) assets)
