-- | Utilities to building transactions on top of the cardano-api.
module Hydra.Ledger.Cardano.Builder where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Data.Default (def)
import qualified Data.Map as Map

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
  deriving (Show)

instance Exception InvalidTransactionException

-- * Constructing

-- | An empty 'TxBodyContent' with all empty/zero values to be extended using
-- record updates.
--
-- FIXME: 'makeTransactionBody' throws when one tries to build a transaction
-- with scripts but no collaterals. This is unfortunate because collaterals are
-- currently added after by out integrated wallet... We may want to revisit our
-- flow to avoid this exception and have the wallet work from a TxBodyContent BuildTx  instead
-- of fiddling with a sealed 'CardanoTx'.
--
-- So we currently bypass this by having default but seemingly innofensive
-- values for collaterals as the 'empty' value
emptyTxBody :: TxBodyContent BuildTx
emptyTxBody =
  TxBodyContent
    { txIns = mempty
    , txInsCollateral = TxInsCollateral mempty -- FIXME
    , txInsReference = TxInsReferenceNone
    , txOuts = mempty
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = TxFeeExplicit 0
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintValueNone
    , txScriptValidity = TxScriptValidityNone
    }

-- | Add new inputs to an ongoing builder.
addInputs :: TxIns BuildTx -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addInputs ins tx =
  tx{txIns = txIns tx <> ins}

addReferenceInputs :: [TxIn] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addReferenceInputs refs' tx =
  tx
    { txInsReference = case txInsReference tx of
        TxInsReferenceNone ->
          TxInsReference refs'
        TxInsReference refs ->
          TxInsReference (refs <> refs')
    }

-- | Like 'addInputs' but only for vk inputs which requires no additional data.
addVkInputs :: [TxIn] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addVkInputs ins =
  addInputs ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

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

-- | Mint tokens with given plutus minting script and redeemer.
mintTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
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
burnTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> [(AssetName, Quantity)] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
burnTokens script redeemer assets =
  mintTokens script redeemer (fmap (second negate) assets)

-- | Set the upper validity bound for this transaction to some 'SlotNo'.
setValidityUpperBound :: SlotNo -> TxBodyContent BuildTx -> TxBodyContent BuildTx
setValidityUpperBound slotNo tx =
  tx{txValidityRange = (lower, TxValidityUpperBound slotNo)}
 where
  (lower, _upper) = txValidityRange tx

-- | Set the lower validity bound for this transaction to some 'SlotNo'.
setValidityLowerBound :: SlotNo -> TxBodyContent BuildTx -> TxBodyContent BuildTx
setValidityLowerBound slotNo tx =
  tx{txValidityRange = (TxValidityLowerBound slotNo, upper)}
 where
  (_lower, upper) = txValidityRange tx

-- | Set the 'txProtocolParams' tx body content field
setProtocolParams :: PParams LedgerEra -> TxBodyContent BuildTx -> TxBodyContent BuildTx
setProtocolParams pparams txBodyC =
  txBodyC
    { txProtocolParams =
        BuildTxWith $ Just $ fromLedgerPParams ShelleyBasedEraBabbage pparams
    }

-- | Build a 'Tx' using default 'ProtocolParameters'
-- NOTE: should be used only in tests
unsafeBuildWithDefaultPParams :: TxBodyContent BuildTx -> Tx
unsafeBuildWithDefaultPParams partialTx =
  unsafeBuildTransaction $ partialTx{txProtocolParams = BuildTxWith $ Just $ fromLedgerPParams ShelleyBasedEraBabbage def}
