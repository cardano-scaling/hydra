{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hydra.OnChainTransaction.Types where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- TODO: Remove dependency on types from contract
import Hydra.ContractStateMachine (
  VerificationKey (..),
 )

newtype MonetaryPolicyId = MonetaryPolicyId ByteString deriving (Eq, Ord)

data Address
  = PubKeyAddress VerificationKey
  | ScriptAddress ByteString

newtype DatumHash = DatumHash ByteString

newtype TxId = TxId ByteString
  deriving (Eq, Show)

data Transaction = Transaction
  { outputs :: [TransactionOutput]
  , inputs :: [TransactionInput]
  }

assetNames :: MonetaryPolicyId -> Transaction -> Set AssetName
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
  , tokens :: Map MonetaryPolicyId (Map AssetName Quantity)
  }

newtype Quantity = Quantity Natural
  deriving (Eq, Num)

data TransactionInput = TransactionInput
  { txId :: TxId
  , utxoIndex :: Integer
  }
  deriving (Eq, Show)

mkTransactionInput :: ByteString -> Integer -> TransactionInput
mkTransactionInput txRef = TransactionInput (TxId txRef)

outputRef :: TransactionInput -> (TxId, Integer)
outputRef TransactionInput{txId, utxoIndex} = (txId, utxoIndex)

data HeadParameters = HeadParameters
  { verificationKeys :: [VerificationKey]
  , -- | This input consumes a UTxO that will be used to uniquely identify the monetary policy
    -- and pay the fees.
    monetaryPolicyInput :: TransactionInput
  }
  deriving (Eq, Show)

newtype MonetaryScript = AnyOf [VerificationKey]
  deriving (Eq, Show)

type AssetId = (MonetaryPolicyId, AssetName)

newtype AssetName = AssetName ByteString deriving (Eq, Ord)
