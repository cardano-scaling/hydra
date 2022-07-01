-- | A concise interface for building transactions, built on top of the
-- cardano-api.
module Hydra.Ledger.Cardano.Builder where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.Default (def)
import qualified Data.Map as Map

-- * Types

type TxBuilder = TxBodyContent BuildTx

-- * Executing

-- | Construct a transction from a builder. It is said 'unsafe' because the
-- underlying implementation will perform some sanity check on a transaction;
-- for example, check that it has at least one input, that no outputs are
-- negatives and whatnot.
--
-- We use the builder only internally for on-chain transaction crafted in the
-- context of Hydra.
unsafeBuildTransaction :: HasCallStack => TxBuilder -> Tx
unsafeBuildTransaction builder =
  either
    (\txBodyError -> bug $ InvalidTransactionException{txBodyError, builder})
    (`Tx` mempty)
    . makeTransactionBody
    $ builder

-- | A runtime exception to capture (programmer) failures when building
-- transactions. This should never happened in practice (famous last words...)!
data InvalidTransactionException = InvalidTransactionException
  { txBodyError :: TxBodyError
  , builder :: TxBuilder
  }
  deriving (Show)

instance Exception InvalidTransactionException

-- * Constructing

-- | An empty 'TxBodyContent' with all empty/zero values to be extended using
-- record updates.
--
-- FIXME: 'makeTransactionBody' throws when one tries to build a transaction
-- with scripts but no collaterals. This is unfortunate because collaterals are
-- currently added after by out integrated wallet... We may want to revisit our
-- flow to avoid this exception and have the wallet work from a TxBuilder instead
-- of fiddling with a sealed 'CardanoTx'.
--
-- Similarly, 'makeTransactionBody' throws when building a transaction
-- with scripts and no protocol parameters (needed to compute the script
-- integrity hash). This is also added by our wallet at the moment so
-- hopefully, this ugly work-around will be removed eventually.
--
-- So we currently bypass this by having default but seemingly innofensive
-- values for collaterals and protocol params in the 'empty' value
emptyTxBody :: TxBuilder
emptyTxBody =
  TxBodyContent
    mempty -- inputs
    (TxInsCollateral mempty) -- FIXME
    TxInsReferenceNone
    mempty -- outputs
    TxTotalCollateralNone
    TxReturnCollateralNone
    (TxFeeExplicit 0)
    (TxValidityNoLowerBound, TxValidityNoUpperBound)
    TxMetadataNone
    TxAuxScriptsNone
    TxExtraKeyWitnessesNone
    (BuildTxWith $ Just $ fromLedgerPParams ShelleyBasedEraBabbage def) -- FIXME
    TxWithdrawalsNone
    TxCertificatesNone
    TxUpdateProposalNone
    TxMintValueNone
    TxScriptValidityNone

-- | Add new inputs to an ongoing builder.
addInputs :: TxIns BuildTx -> TxBuilder -> TxBuilder
addInputs ins tx =
  tx{txIns = txIns tx <> ins}

-- | Like 'addInputs' but only for vk inputs which requires no additional data.
addVkInputs :: [TxIn] -> TxBuilder -> TxBuilder
addVkInputs ins =
  addInputs ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

-- | Append new outputs to an ongoing builder.
addOutputs :: [TxOut CtxTx] -> TxBuilder -> TxBuilder
addOutputs outputs tx =
  tx{txOuts = txOuts tx <> outputs}

-- | Add extra required key witnesses to a transaction.
addExtraRequiredSigners :: [Hash PaymentKey] -> TxBuilder -> TxBuilder
addExtraRequiredSigners vks tx =
  tx{txExtraKeyWits = txExtraKeyWits'}
 where
  txExtraKeyWits' =
    case txExtraKeyWits tx of
      TxExtraKeyWitnessesNone ->
        TxExtraKeyWitnesses vks
      TxExtraKeyWitnesses vks' ->
        TxExtraKeyWitnesses (vks' <> vks)

-- | Mint tokens with given plutus minting script and redeemer.
mintTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBuilder -> TxBuilder
mintTokens script redeemer assets tx =
  tx{txMintValue = TxMintValue mintedTokens' mintedWitnesses'}
 where
  (mintedTokens, mintedWitnesses) =
    case txMintValue tx of
      TxMintValueNone ->
        (mempty, mempty)
      TxMintValue t (BuildTxWith m) ->
        (t, m)

  mintedTokens' =
    mintedTokens <> valueFromList (fmap (first (AssetId policyId)) assets)

  mintedWitnesses' =
    BuildTxWith $ mintedWitnesses <> Map.singleton policyId mintingWitness

  mintingWitness =
    mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)

  policyId =
    PolicyId $ hashScript $ PlutusScript script

-- | Burn tokens with given plutus minting script and redeemer.
-- This is really just `mintTokens` with negated 'Quantity'.
burnTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBuilder -> TxBuilder
burnTokens script redeemer assets =
  mintTokens script redeemer (fmap (second negate) assets)

-- | Set the upper validity bound for this transaction to some 'SlotNo'.
setValidityUpperBound :: SlotNo -> TxBuilder -> TxBuilder
setValidityUpperBound slotNo tx =
  tx{txValidityRange = (lower, TxValidityUpperBound slotNo)}
 where
  (lower, _upper) = txValidityRange tx

-- | Set the lower validity bound for this transaction to some 'SlotNo'.
setValidityLowerBound :: SlotNo -> TxBuilder -> TxBuilder
setValidityLowerBound slotNo tx =
  tx{txValidityRange = (TxValidityLowerBound slotNo, upper)}
 where
  (_lower, upper) = txValidityRange tx
