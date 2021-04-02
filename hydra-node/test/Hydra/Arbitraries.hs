module Hydra.Arbitraries where

import Cardano.Prelude
import qualified Data.ByteString as ByteString
import Hydra.ContractStateMachine (VerificationKey (..))
import Hydra.OnChainTransaction
import Test.QuickCheck

newtype SomeHeadParameters = SomeHeadParameters HeadParameters
  deriving (Eq, Show)

instance Arbitrary SomeHeadParameters where
  arbitrary = SomeHeadParameters <$> generateHeadParameters

generateHeadParameters :: Gen HeadParameters
generateHeadParameters =
  HeadParameters <$> listOf generateVerificationKey <*> generateTransactionInput

generateTransactionInput :: Gen TransactionInput
generateTransactionInput =
  TransactionInput <$> generateTxId <*> (getSmall . getPositive <$> arbitrary)

generateTxId :: Gen TxId
generateTxId = TxId . ByteString.pack <$> vectorOf 16 arbitrary

generateVerificationKey :: Gen VerificationKey
generateVerificationKey = VerificationKey . ByteString.pack <$> vectorOf 32 arbitrary
