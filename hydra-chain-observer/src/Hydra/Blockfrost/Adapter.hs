module Hydra.Blockfrost.Adapter where

import Hydra.Prelude

import Blockfrost.Client (
  Block (..),
  Transaction (..),
  UtxoOutput (..),
  unBlockHash,
  unSlot,
 )
import Blockfrost.Client qualified as Blockfrost
import Cardano.Chain.Common (decodeAddressBase58)
import Hydra.Cardano.Api (Address (..), CtxUTxO, Era, HasTypeProxy (..), Hash, SlotNo, TxId (..), TxIx (..), Value, mkTxOutValue, unsafeScriptDataHashFromBytes, unsafeScriptHashFromBytes)
import Hydra.Cardano.Api.Prelude (
  AddressInEra,
  AssetId (..),
  AssetName (..),
  BlockHeader (..),
  ChainPoint (..),
  HashableScriptData,
  PolicyId (..),
  Quantity (..),
  ReferenceScript (..),
  SerialiseAsCBOR (..),
  TxOut (..),
  TxOutDatum (..),
  TxOutValue,
  alonzoBasedEra,
  babbageBasedEra,
  byronAddressInEra,
  unsafeHashFromBytes,
  valueFromList,
 )
import Money qualified

txIx :: Blockfrost.Transaction -> TxIx
txIx Blockfrost.Transaction{_transactionIndex} =
  TxIx $ fromIntegral _transactionIndex

txId :: Blockfrost.Transaction -> TxId
txId Blockfrost.Transaction{_transactionHash, _transactionIndex} =
  TxId . unsafeHashFromBytes . encodeUtf8 $ _transactionHash

txOuts :: Blockfrost.UtxoOutput -> [TxOut CtxUTxO Era]
txOuts
  Blockfrost.UtxoOutput
    { _utxoOutputAddress
    , _utxoOutputAmount
    , _utxoOutputDataHash
    , _utxoOutputOutputIndex
    , _utxoOutputCollateral
    , _utxoOutputInlineDatum
    , _utxoOutputReferenceScriptHash
    } =
    ( \txOutValue ->
        TxOut txOutAddress txOutValue txOutDatum txOutReferenceScript
    )
      <$> txOutValues
   where
    txOutAddress :: AddressInEra Era
    txOutAddress =
      toAddress _utxoOutputAddress

    txOutValues :: [TxOutValue Era]
    txOutValues =
      mkTxOutValue . toValue <$> _utxoOutputAmount

    txOutDatum :: TxOutDatum CtxUTxO Era
    txOutDatum =
      fromMaybe TxOutDatumNone $
        toDatumHash <$> _utxoOutputDataHash
          <|> (toInlineDatum <$> _utxoOutputInlineDatum)

    txOutReferenceScript :: ReferenceScript Era
    txOutReferenceScript =
      maybe
        ReferenceScriptNone
        toReferenceScript
        _utxoOutputReferenceScriptHash

-- FIXME! https://blockfrost.dev/api/specific-script
toReferenceScript :: Blockfrost.ScriptHash -> ReferenceScript Era
toReferenceScript = const ReferenceScriptNone

toAddress :: Blockfrost.Address -> AddressInEra Era
toAddress (Blockfrost.Address addr) =
  case decodeAddressBase58 addr of
    Left err -> error $ "Bad Base58 address: " <> show err
    Right byronAddress -> byronAddressInEra $ ByronAddress byronAddress

toDatumHash :: Blockfrost.DatumHash -> TxOutDatum CtxUTxO Era
toDatumHash (Blockfrost.DatumHash datum) =
  TxOutDatumHash
    (alonzoBasedEra @Era)
    (unsafeScriptDataHashFromBytes . encodeUtf8 $ datum)

toInlineDatum :: Blockfrost.InlineDatum -> TxOutDatum CtxUTxO Era
toInlineDatum (Blockfrost.InlineDatum (Blockfrost.ScriptDatumCBOR datumCbor)) =
  case decodeBase16 datumCbor of
    Left decodeErr -> error $ "Bad Base16 InlineDatum CBOR: " <> decodeErr
    Right bytes ->
      case deserialiseFromCBOR (proxyToAsType (Proxy @HashableScriptData)) bytes of
        Left deserializeErr -> error $ "Bad InlineDatum CBOR: " <> show deserializeErr
        Right scriptDataCbor -> TxOutDatumInline (babbageBasedEra @Era) scriptDataCbor

toValue :: Blockfrost.Amount -> Value
toValue = \case
  Blockfrost.AdaAmount lovelaces ->
    valueFromList
      [
        ( AdaAssetId
        , Quantity (toInteger lovelaces)
        )
      ]
  Blockfrost.AssetAmount sd ->
    let currency = Money.someDiscreteCurrency sd
     in valueFromList
          [
            ( AssetId
                (PolicyId . unsafeScriptHashFromBytes $ encodeUtf8 currency)
                (AssetName $ encodeUtf8 currency)
            , Quantity (Money.someDiscreteAmount sd)
            )
          ]

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ unBlockHash _blockHash
