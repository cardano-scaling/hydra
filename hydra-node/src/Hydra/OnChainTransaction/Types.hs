{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hydra.OnChainTransaction.Types where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- TODO: Remove dependency on types from contract
import Hydra.ContractStateMachine (
  VerificationKey (..),
 )

newtype PolicyId = PolicyId ByteString deriving (Eq, Ord)

data Address
  = PubKeyAddress VerificationKey
  | ScriptAddress ByteString

newtype DatumHash = DatumHash ByteString

newtype TxId = TxId ByteString

data Transaction = Transaction
  { outputs :: [TransactionOutput]
  , inputs :: [TransactionInput]
  }

assetNames :: PolicyId -> Transaction -> Set AssetName
assetNames policyId =
  Set.fromList
    . concatMap Map.keys
    . mapMaybe (Map.lookup policyId . tokens . value)
    . outputs

getStateMachineOutput :: Transaction -> Maybe TransactionOutput
getStateMachineOutput = find (isJust . datum) . outputs

data TransactionOutput = TransactionOutput
  { value :: Value
  , address :: Address
  , datum :: Maybe DatumHash
  }

data Value = Value
  { adas :: Quantity
  , tokens :: Map PolicyId (Map AssetName Quantity)
  }

newtype Quantity = Quantity Natural
  deriving newtype (Eq, Num)

data TransactionInput = TransactionInput {txId :: TxId, utxoIndex :: Integer}

mkTransactionInput :: ByteString -> Integer -> TransactionInput
mkTransactionInput txRef = TransactionInput (TxId txRef)

outputRef :: TransactionInput -> (TxId, Integer)
outputRef TransactionInput{txId, utxoIndex} = (txId, utxoIndex)

data HeadParameters = HeadParameters
  { verificationKeys :: [VerificationKey]
  , monetaryPolicyInput :: TransactionInput
  }

newtype MonetaryScript = AnyOf [VerificationKey]

type AssetId = (PolicyId, AssetName)

newtype AssetName = AssetName ByteString deriving (Eq, Ord)
