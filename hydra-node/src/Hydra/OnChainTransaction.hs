{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Top-level module to run a single Hydra node
module Hydra.OnChainTransaction (
  buildInitialTransaction,
  initialState,
  module Hydra.OnChainTransaction.Types,
  module Hydra.OnChainTransaction.Plutus,
) where

import Cardano.Prelude
import qualified Data.Map.Strict as Map hiding (map)
import Hydra.ContractStateMachine (
  Eta (..),
  HydraState (..),
  MultisigPublicKey (..),
  OpenState (..),
  UTXO (..),
  VerificationKey (..),
  contractAddress,
  toDatumHash,
 )
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.OnChainTransaction.Plutus
import Hydra.OnChainTransaction.Types

buildInitialTransaction :: HeadParameters -> (Transaction, PolicyId)
buildInitialTransaction HeadParameters{verificationKeys, monetaryPolicyInput} =
  let policyId = fromCurrencySymbol $ hydraCurrencySymbol (first toTxId $ outputRef monetaryPolicyInput)
      mkOutput verificationKey =
        TransactionOutput
          { value =
              Value
                (Quantity 0)
                (Map.fromList [(policyId, Map.fromList [(AssetName (unverificationKey verificationKey), Quantity 1)])])
          , address = PubKeyAddress verificationKey
          , datum = Nothing
          }
      stateMachineOutput =
        TransactionOutput
          { value = Value 0 mempty
          , address = fromPlutusAddress contractAddress
          , datum = Just (fromPlutusDatumHash $ toDatumHash (initialState verificationKeys))
          }
      outputs = stateMachineOutput : map mkOutput verificationKeys
      inputs = [monetaryPolicyInput]
   in (Transaction{outputs, inputs}, policyId)

--
-- Hydra State
--

initialState :: [VerificationKey] -> HydraState
initialState keys = Open openState
 where
  openState =
    OpenState
      { keyAggregate = MultisigPublicKey keys
      , eta =
          Eta
            { utxos = UTXO
            , snapshotNumber = 0
            , transactions = mempty
            }
      }
