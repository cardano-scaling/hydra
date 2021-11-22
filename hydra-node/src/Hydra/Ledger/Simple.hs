-- | A mock implementation of a ledger slightly less dumb than 'Mock'.
--
-- This implementation of a 'Ledger' adds a bit more logic in order to:
--
-- * Be able to have a representation of 'UTxO' closer to what a real-life eUTxO would be,
--   so that we can distinguish it from other components of the ledger,
-- * Be able to have transactions validated against the current state of the ledger, so that
--   one transaction could at some point be invalid, then becomes valid because some inputs it
--   consumes is now available.
module Hydra.Ledger.Simple where

import Hydra.Prelude

import Cardano.Api
import Cardano.Binary (decodeFullDecoder, serializeEncoding')
import Data.Aeson (
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.List (maximum)
import qualified Data.Set as Set
import Hydra.Ledger
import Test.QuickCheck (choose, getSize, sublistOf)

--
-- SimpleTx
--

-- | Simple transaction.
-- A transaction is a 'SimpleId', a list of inputs and a list of outputs.
data SimpleTx = SimpleTx
  { txSimpleId :: SimpleId
  , txInputs :: UtxoType SimpleTx
  , txOutputs :: UtxoType SimpleTx
  }
  deriving stock (Eq, Ord, Generic, Show)

type SimpleId = Integer

instance IsTx SimpleTx where
  type UtxoType SimpleTx = Set MockTxIn
  type TxIdType SimpleTx = SimpleId
  type ValueType SimpleTx = Int

  txId (SimpleTx tid _ _) = tid
  balance = Set.size

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

instance SerialiseAsCBOR SimpleTx where
  serialiseToCBOR (SimpleTx txid inputs outputs) =
    serializeEncoding' (toCBOR txid <> toCBOR inputs <> toCBOR outputs)
  deserialiseFromCBOR _ =
    decodeFullDecoder
      "SimpleTx"
      ( SimpleTx
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )
      . fromStrict

data AsSimpleTx
instance HasTypeProxy SimpleTx where
  data AsType SimpleTx = AsSimpleTx
  proxyToAsType _ = AsSimpleTx

--
-- MockTxIn
--

-- |An identifier for a single output of a 'SimpleTx'.
newtype MockTxIn = MockTxIn {unMockTxIn :: Integer}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON)

instance Arbitrary MockTxIn where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance ToCBOR MockTxIn where
  toCBOR (MockTxIn inId) = toCBOR inId

instance FromCBOR MockTxIn where
  fromCBOR = MockTxIn <$> fromCBOR

simpleLedger :: Ledger SimpleTx
simpleLedger =
  Ledger
    { applyTransactions =
        foldlM $ \utxo (SimpleTx _ ins outs) ->
          if ins `Set.isSubsetOf` utxo && utxo `Set.disjoint` outs
            then Right $ (utxo Set.\\ ins) `Set.union` outs
            else Left $ ValidationError "cannot apply transaction"
    , initUtxo = mempty
    }

--
-- Builders
--

utxoRef :: Integer -> UtxoType SimpleTx
utxoRef = Set.singleton . MockTxIn

utxoRefs :: [Integer] -> UtxoType SimpleTx
utxoRefs = Set.fromList . fmap MockTxIn

aValidTx :: Integer -> SimpleTx
aValidTx n = SimpleTx n mempty (utxoRef n)

--
--  Generators
--

listOfCommittedUtxos :: Integer -> Gen [UtxoType SimpleTx]
listOfCommittedUtxos numCommits =
  pure $ Set.singleton . MockTxIn <$> [1 .. numCommits]

genSequenceOfValidTransactions :: UtxoType SimpleTx -> Gen [SimpleTx]
genSequenceOfValidTransactions initialUtxo = do
  n <- fromIntegral <$> getSize
  let maxId = if Set.null initialUtxo then 0 else unMockTxIn (maximum initialUtxo)
  numTxs <- choose (1, n)
  foldlM newTx (maxId, initialUtxo, mempty) [1 .. numTxs] >>= \(_, _, txs) -> pure (reverse txs)
 where
  newTx ::
    (TxIdType SimpleTx, UtxoType SimpleTx, [SimpleTx]) ->
    TxIdType SimpleTx ->
    Gen (TxIdType SimpleTx, UtxoType SimpleTx, [SimpleTx])
  newTx (maxId, utxo, txs) txid = do
    (newMax, ins, outs) <- genInputsAndOutputs maxId utxo
    pure (newMax, (utxo Set.\\ ins) `Set.union` outs, SimpleTx txid ins outs : txs)

  genInputsAndOutputs ::
    TxIdType SimpleTx ->
    Set MockTxIn ->
    Gen (TxIdType SimpleTx, Set MockTxIn, Set MockTxIn)
  genInputsAndOutputs maxId utxo = do
    ins <- sublistOf (Set.toList utxo)
    numOuts <- choose (1, 10)
    let outs = fmap (+ maxId) [1 .. numOuts]
    pure (maximum outs, Set.fromList ins, Set.fromList $ fmap MockTxIn outs)
