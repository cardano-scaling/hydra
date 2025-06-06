-- | Utilities to building transactions on top of the cardano-api.
module Hydra.Ledger.Cardano.Builder where

import Data.MonoTraversable (omap)
import Hydra.Cardano.Api
import Hydra.Prelude

import Data.Map qualified as Map

-- * Executing

-- | Construct a transaction from a builder. It is said 'unsafe' because the
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

-- | Mint tokens with given plutus minting script and redeemer.
mintTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> PolicyAssets -> TxBodyContent BuildTx -> TxBodyContent BuildTx
mintTokens script redeemer assets = addTxMintValue newAssets
 where
  newAssets :: Map PolicyId (PolicyAssets, BuildTxWith BuildTx (ScriptWitness WitCtxMint))
  newAssets = Map.fromList [(policyId, (assets, BuildTxWith mintingWitness))]

  mintingWitness :: ScriptWitness WitCtxMint
  mintingWitness =
    mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)

  policyId :: PolicyId
  policyId =
    PolicyId $ hashScript $ PlutusScript script

-- | Burn tokens with given plutus minting script and redeemer.
-- This is really just `mintTokens` with negated 'Quantity'.
burnTokens :: ToScriptData redeemer => PlutusScript -> redeemer -> PolicyAssets -> TxBodyContent BuildTx -> TxBodyContent BuildTx
burnTokens script redeemer assets =
  mintTokens script redeemer $ omap negate assets
