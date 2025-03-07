module Hydra.Chain.Blockfrost.Client where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Time.Clock.POSIX
import Hydra.Cardano.Api hiding (fromNetworkMagic)

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecode', encode)
import Data.Set qualified as Set
import Hydra.Cardano.Api.Prelude (StakePoolKey)
import Hydra.Contract.Head qualified as Head
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Money (someDiscreteAmount, someDiscreteCurrency)

-- import Money qualified

data APIBlockfrostError
  = BlockfrostError Text
  | DecodeError Text
  deriving (Show, Exception)

runBlockfrostM ::
  (MonadIO m, MonadThrow m) =>
  Blockfrost.Project ->
  BlockfrostClientT IO a ->
  m a
runBlockfrostM prj action = do
  result <- liftIO $ runBlockfrost prj action
  case result of
    Left err -> throwIO (BlockfrostError $ show err)
    Right val -> pure val

publishHydraScripts ::
  -- | The path where the Blockfrost project token hash is stored.
  FilePath ->
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  IO [TxId]
publishHydraScripts projectPath sk = do
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    pparams' <- Blockfrost.getLatestEpochProtocolParams
    case eitherDecode' (encode pparams') :: Either String (PParams LedgerEra) of
      Left err -> liftIO $ throwIO (DecodeError $ toText err)
      Right pparams -> do
        epoch <- Blockfrost.getLatestEpoch
        Blockfrost.Genesis
          { _genesisNetworkMagic = networkMagic
          , _genesisSystemStart = systemStart
          } <-
          Blockfrost.getLedgerGenesis
        let address = Blockfrost.Address (vkAddress networkMagic)
        let networkId = fromNetworkMagic networkMagic
        let changeAddress = mkVkAddress networkId vk
        stakePools <- Blockfrost.listPools
        forM scripts $ \script -> do
          utxo <- Blockfrost.getAddressUtxos address
          liftIO $
            buildTx pparams undefined networkId systemStart stakePools script changeAddress utxo
              >>= \case
                Left err ->
                  liftIO $ throwErrorAsException err
                Right rawTx -> do
                  let body = getTxBody rawTx
                      tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
                      -- REVIEW! double CBOR encoding
                      txByteString :: BL.ByteString = toLazyByteString (CBOR.encodeBytes $ serialiseToCBOR tx)
                      txCborString = Blockfrost.CBORString txByteString
                  txHash <- Blockfrost.submitTx txCborString
                  -- TODO! await transaction confirmed
                  pure undefined
 where
  scripts = [initialValidatorScript, commitValidatorScript, Head.validatorScript]

  vk = getVerificationKey sk

  vkAddress networkMagic = textAddrOf (fromNetworkMagic networkMagic) vk

-- TODO!
buildTx ::
  PParams LedgerEra ->
  LedgerEpochInfo ->
  NetworkId ->
  POSIXTime ->
  [Blockfrost.PoolId] ->
  PlutusScript ->
  -- | Change address to send
  AddressInEra ->
  [Blockfrost.AddressUtxo] ->
  IO (Either (TxBodyErrorAutoBalance Era) Tx)
buildTx pparams epochInfo networkId posixTime stakePools script changeAddress utxo = do
  pure $
    second (flip Tx [] . balancedTxBody) $
      makeTransactionBodyAutoBalance
        shelleyBasedEra
        systemStart
        epochInfo
        (LedgerProtocolParameters pparams)
        (Set.fromList (toApiPoolId <$> stakePools))
        mempty
        mempty
        (UTxO.toApi utxoToSpend)
        bodyContent
        changeAddress
        Nothing
 where
  unspendableScriptAddress = mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
  -- FIXME! mkTxOutAutoBalance with minUTxOValue from pparams
  outputs = TxOut unspendableScriptAddress mempty TxOutDatumNone <$> [mkScriptRef script]
  utxo' = toApiUTxO utxo changeAddress
  totalDeposit = sum (selectLovelace . txOutValue <$> outputs)
  utxoToSpend = maybe mempty UTxO.singleton $ UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo'
  systemStart = SystemStart $ posixSecondsToUTCTime posixTime
  collateral = mempty
  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  dummyFeeForBalancing = TxFeeExplicit 0
  bodyContent =
    TxBodyContent
      (withWitness <$> toList (UTxO.inputSet utxoToSpend))
      (TxInsCollateral collateral)
      TxInsReferenceNone
      outputs
      TxTotalCollateralNone
      TxReturnCollateralNone
      dummyFeeForBalancing
      TxValidityNoLowerBound
      TxValidityNoUpperBound
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith $ Just $ LedgerProtocolParameters pparams)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintValueNone
      TxScriptValidityNone
      Nothing
      Nothing
      Nothing
      Nothing

-- ** Extras

toApiPoolId :: Blockfrost.PoolId -> Hash StakePoolKey
toApiPoolId (Blockfrost.PoolId textPoolId) =
  case deserialiseFromRawBytesHex (AsHash AsStakePoolKey) (encodeUtf8 textPoolId) of
    Left err -> error (show err)
    Right pool -> pool

toApiUTxO :: [Blockfrost.AddressUtxo] -> AddressInEra -> UTxO' (TxOut CtxUTxO)
toApiUTxO utxos addr = UTxO.fromPairs (toEntry <$> utxos)
 where
  toEntry :: Blockfrost.AddressUtxo -> (TxIn, TxOut CtxUTxO)
  toEntry utxo = (toApiTxIn utxo, toApiTxOut utxo addr)

toApiTxIn :: Blockfrost.AddressUtxo -> TxIn
toApiTxIn Blockfrost.AddressUtxo{_addressUtxoTxHash = Blockfrost.TxHash{unTxHash}, _addressUtxoOutputIndex} =
  case deserialiseFromRawBytesHex AsTxId (encodeUtf8 unTxHash) of
    Left err -> error (show err)
    Right txId -> TxIn txId (TxIx (fromIntegral _addressUtxoOutputIndex))

-- REVIEW! TxOutDatumNone and ReferenceScriptNone
toApiTxOut :: Blockfrost.AddressUtxo -> AddressInEra -> TxOut CtxUTxO
toApiTxOut Blockfrost.AddressUtxo{_addressUtxoAmount} addr =
  TxOut addr (toApiValue _addressUtxoAmount) TxOutDatumNone ReferenceScriptNone

toApiPolicyId :: Text -> PolicyId
toApiPolicyId pid =
  case deserialiseFromRawBytesHex AsPolicyId (encodeUtf8 pid) of
    Left err -> error (show err)
    Right p -> p

toApiAssetName :: Text -> AssetName
toApiAssetName = AssetName . encodeUtf8

toApiValue :: [Blockfrost.Amount] -> Value
toApiValue = foldMap convertAmount
 where
  convertAmount (Blockfrost.AdaAmount lovelaces) =
    fromList
      [
        ( AdaAssetId
        , Quantity (toInteger lovelaces)
        )
      ]
  convertAmount (Blockfrost.AssetAmount money) =
    let currency = someDiscreteCurrency money
     in fromList
          [
            ( AssetId
                (toApiPolicyId currency)
                (toApiAssetName currency)
            , Quantity (someDiscreteAmount money)
            )
          ]

-- ** Helpers

unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."

textAddrOf :: NetworkId -> VerificationKey PaymentKey -> Text
textAddrOf networkId vk = unwrapAddress (mkVkAddress @Era networkId vk)

fromNetworkMagic :: Integer -> NetworkId
fromNetworkMagic = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))
