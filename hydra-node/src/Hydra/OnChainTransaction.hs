module Hydra.OnChainTransaction (
  buildInitialTransaction,
  module Hydra.OnChainTransaction.Types,
  module Hydra.OnChainTransaction.State,
  module Hydra.OnChainTransaction.Plutus,
) where

import Cardano.Prelude
import qualified Data.Map.Strict as Map hiding (map)

-- TODO: Remove dependency on types from contract
import Hydra.ContractStateMachine (
  VerificationKey (..),
  contractAddress,
  toDatumHash,
 )
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.OnChainTransaction.Plutus
import Hydra.OnChainTransaction.State
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
