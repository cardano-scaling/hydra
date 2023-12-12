-- | A mock implementation of a ledger using very simple UTxO transactions.
--
-- These transactions have a very simplified representation of unspent
-- transaction outputs being just integers, but already have inputs and outputs.
-- Transactions are validated against the current state of the ledger, so that
-- one transaction could at some point be invalid, then becomes valid because
-- some inputs it consumes are now available.
--
-- NOTE: There is no notion of time in this ledger, so transactions validation
-- will never depend on the L1 slot.
module Hydra.Ledger.Simple where

import Hydra.Prelude

import Codec.Serialise (serialise)
import Data.Aeson (
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.List (maximum)
import Data.Set qualified as Set
import Hydra.Chain (ChainStateType, IsChainState (..))
import Hydra.Ledger (
  ChainSlot (..),
  IsTx (..),
  Ledger (..),
  ValidationError (ValidationError),
 )
import Test.QuickCheck (choose, getSize, sublistOf)

-- * Simple transactions

-- | Simple transaction.
-- A transaction is a 'SimpleId', a list of inputs and a list of outputs,
-- and it has no time validity.
data SimpleTx = SimpleTx
  { txSimpleId :: SimpleId
  , txInputs :: UTxOType SimpleTx
  , txOutputs :: UTxOType SimpleTx
  }
  deriving stock (Eq, Ord, Generic, Show)

type SimpleId = Integer

instance IsTx SimpleTx where
  type UTxOType SimpleTx = Set SimpleTxIn
  type TxIdType SimpleTx = SimpleId
  type ValueType SimpleTx = Int

  txId (SimpleTx tid _ _) = tid
  balance = Set.size
  hashUTxO = toStrict . foldMap (serialise . unSimpleTxIn)
  utxoFromTx = txOutputs
  withoutUTxO = Set.difference

  txSpendingUTxO utxo =
    SimpleTx
      { txSimpleId = 0
      , txInputs = utxo
      , txOutputs = mempty
      }

instance Arbitrary SimpleTx where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToJSON SimpleTx where
  toJSON tx =
    object
      [ "id" .= txId tx
      , "inputs" .= txInputs tx
      , "outputs" .= txOutputs tx
      ]

instance FromJSON SimpleTx where
  parseJSON = withObject "SimpleTx" $ \obj ->
    SimpleTx
      <$> (obj .: "id")
      <*> (obj .: "inputs")
      <*> (obj .: "outputs")

instance ToCBOR SimpleTx where
  toCBOR (SimpleTx txid inputs outputs) =
    toCBOR txid <> toCBOR inputs <> toCBOR outputs

instance FromCBOR SimpleTx where
  fromCBOR =
    SimpleTx
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

-- * Simple chain state

newtype SimpleChainState = SimpleChainState {slot :: ChainSlot}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary SimpleChainState where
  arbitrary = SimpleChainState <$> arbitrary

instance IsChainState SimpleTx where
  type ChainStateType SimpleTx = SimpleChainState

  chainStateSlot SimpleChainState{slot} = slot

--
-- MockTxIn
--

-- | An identifier for a single output of a 'SimpleTx'.
newtype SimpleTxIn = SimpleTxIn {unSimpleTxIn :: Integer}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON)

instance Arbitrary SimpleTxIn where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToCBOR SimpleTxIn where
  toCBOR (SimpleTxIn inId) = toCBOR inId

instance FromCBOR SimpleTxIn where
  fromCBOR = SimpleTxIn <$> fromCBOR

simpleLedger :: Ledger SimpleTx
simpleLedger =
  Ledger{applyTransactions}
 where
  -- NOTE: _slot is unused as SimpleTx transactions don't have a notion of time.
  applyTransactions _slot =
    foldlM $ \utxo tx@(SimpleTx _ ins outs) ->
      if ins `Set.isSubsetOf` utxo && utxo `Set.disjoint` outs
        then Right $ (utxo Set.\\ ins) `Set.union` outs
        else Left (tx, ValidationError "cannot apply transaction")

--
-- Builders
--

utxoRef :: Integer -> UTxOType SimpleTx
utxoRef = Set.singleton . SimpleTxIn

utxoRefs :: [Integer] -> UTxOType SimpleTx
utxoRefs = Set.fromList . fmap SimpleTxIn

aValidTx :: Integer -> SimpleTx
aValidTx n = SimpleTx n mempty (utxoRef n)

--
--  Generators
--

listOfCommittedUTxOs :: Integer -> Gen [UTxOType SimpleTx]
listOfCommittedUTxOs numCommits =
  pure $ Set.singleton . SimpleTxIn <$> [1 .. numCommits]

genSequenceOfValidTransactions :: UTxOType SimpleTx -> Gen [SimpleTx]
genSequenceOfValidTransactions initialUTxO = do
  n <- fromIntegral <$> getSize
  let maxId = if Set.null initialUTxO then 0 else unSimpleTxIn (maximum initialUTxO)
  numTxs <- choose (1, n)
  foldlM newTx (maxId, initialUTxO, mempty) [1 .. numTxs] >>= \(_, _, txs) -> pure (reverse txs)
 where
  newTx ::
    (TxIdType SimpleTx, UTxOType SimpleTx, [SimpleTx]) ->
    TxIdType SimpleTx ->
    Gen (TxIdType SimpleTx, UTxOType SimpleTx, [SimpleTx])
  newTx (maxId, utxo, txs) txid = do
    (newMax, ins, outs) <- genInputsAndOutputs maxId utxo
    pure (newMax, (utxo Set.\\ ins) `Set.union` outs, SimpleTx txid ins outs : txs)

  genInputsAndOutputs :: Integer -> Set SimpleTxIn -> Gen (Integer, Set SimpleTxIn, Set SimpleTxIn)
  genInputsAndOutputs maxId utxo = do
    ins <- sublistOf (Set.toList utxo)
    numOuts <- choose (1, 10)
    let outs = fmap (+ maxId) [1 .. numOuts]
    pure (maximum outs, Set.fromList ins, Set.fromList $ fmap SimpleTxIn outs)
