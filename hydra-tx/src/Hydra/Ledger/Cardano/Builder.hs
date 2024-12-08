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

addTxIns :: TxIns build -> TxBodyContent build -> TxBodyContent build
addTxIns txIns = modTxIns (<> txIns)

addTxInsSpending :: [TxIn] -> TxBodyContent BuildTx -> TxBodyContent BuildTx
addTxInsSpending txIns = addTxIns ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> txIns)

modTxInsReference :: (TxInsReference -> TxInsReference) -> TxBodyContent build -> TxBodyContent build
modTxInsReference f txBodyContent = txBodyContent{txInsReference = f (txInsReference txBodyContent)}

addTxInsReference :: [TxIn] -> TxBodyContent build -> TxBodyContent build
addTxInsReference txInsReference =
  modTxInsReference
    ( \case
        TxInsReferenceNone -> TxInsReference txInsReference
        TxInsReference xs -> TxInsReference (xs <> txInsReference)
    )

addTxInReference :: TxIn -> TxBodyContent build -> TxBodyContent build
addTxInReference txInReference = addTxInsReference [txInReference]

addTxOuts :: [TxOut CtxTx] -> TxBodyContent build -> TxBodyContent build
addTxOuts txOuts = modTxOuts (<> txOuts)

modTxInsCollateral :: (TxInsCollateral -> TxInsCollateral) -> TxBodyContent build -> TxBodyContent build
modTxInsCollateral f txBodyContent = txBodyContent{txInsCollateral = f (txInsCollateral txBodyContent)}

addTxInsCollateral :: [TxIn] -> TxBodyContent build -> TxBodyContent build
addTxInsCollateral txInsCollateral =
  modTxInsCollateral
    ( \case
        TxInsCollateralNone -> TxInsCollateral txInsCollateral
        TxInsCollateral xs -> TxInsCollateral (xs <> txInsCollateral)
    )

addTxInCollateral :: TxIn -> TxBodyContent build -> TxBodyContent build
addTxInCollateral txInCollateral = addTxInsCollateral [txInCollateral]

modExtraKeyWits :: (TxExtraKeyWitnesses -> TxExtraKeyWitnesses) -> TxBodyContent build -> TxBodyContent build
modExtraKeyWits f txBodyContent = txBodyContent{txExtraKeyWits = f (txExtraKeyWits txBodyContent)}

addExtraKeyWits :: [Hash PaymentKey] -> TxBodyContent build -> TxBodyContent build
addExtraKeyWits vks =
  modExtraKeyWits
    ( \case
        TxExtraKeyWitnessesNone ->
          TxExtraKeyWitnesses vks
        TxExtraKeyWitnesses vks' ->
          TxExtraKeyWitnesses (vks' <> vks)
    )

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
    mintedTokens <> fromList (fmap (first (AssetId policyId)) assets)

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
