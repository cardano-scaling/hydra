{-# LANGUAGE RecordWildCards #-}

module Hydra.Chain.Blockfrost.Client (
  module Hydra.Chain.Blockfrost.Client,
  module Blockfrost.Client,
) where

import Hydra.Prelude

import Blockfrost.Client (
  Block (..),
  BlockHash (..),
  BlockfrostClientT,
  Genesis (..),
  Project,
  Slot (..),
  TransactionCBOR (..),
  TxHashCBOR (..),
  allPages,
  def,
  getBlock,
  getBlockTxsCBOR',
  getLedgerGenesis,
  listPools,
  projectFromFile,
  runBlockfrost,
  tryError,
  unBlockHash,
  unSlot,
 )
import Blockfrost.Client qualified as Blockfrost
import Cardano.Chain.Genesis (mainnetProtocolMagicId)
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX
import Hydra.Cardano.Api hiding (LedgerState, fromNetworkMagic, queryGenesisParameters)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochSize (..), NonNegativeInterval, UnitInterval, boundRational, unsafeNonZero)
import Cardano.Ledger.Binary.Version (mkVersion)
import Cardano.Ledger.Conway.Core (
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  ppCommitteeMaxTermLengthL,
  ppCommitteeMinSizeL,
  ppDRepActivityL,
  ppDRepDepositL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Plutus (ExUnits (..), Language (..), Prices (..))
import Cardano.Ledger.Plutus.CostModels (CostModels, mkCostModel, mkCostModels)
import Cardano.Ledger.Shelley.API (ProtVer (..))
import Cardano.Slotting.Time (RelativeTime (..), mkSlotLength)
import Control.Lens ((.~), (^.))
import Data.List qualified as List
import Data.SOP.NonEmpty (nonEmptyFromList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hydra.Cardano.Api.Prelude (StakePoolKey, fromNetworkMagic)
import Hydra.Tx (ScriptRegistry, newScriptRegistry)
import Money qualified
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..), EraParams (..), EraSummary (..), SafeZone (..), Summary (..), mkInterpreter)

data BlockfrostException
  = TimeoutOnUTxO TxId
  | NoUTxOFound (Address ShelleyAddr)
  | FailedToDecodeAddress Text
  | ByronAddressNotSupported
  | FailedUTxOForHash Text
  | FailedEraHistory
  | AssetNameMissing
  | DeserialiseError Text
  | DecodeError Text
  | BlockfrostAPIError Text
  deriving (Show, Exception)

newtype APIBlockfrostError
  = BlockfrostError BlockfrostException
  deriving newtype (Show, Exception)

runBlockfrostM ::
  (MonadIO m, MonadThrow m) =>
  Blockfrost.Project ->
  BlockfrostClientT IO a ->
  m a
runBlockfrostM prj action = do
  result <- liftIO $ runBlockfrost prj action
  case result of
    Left err -> throwIO $ BlockfrostError $ BlockfrostAPIError (show err)
    Right val -> pure val

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- Can throw at least 'NewScriptRegistryException' on failure.
queryScriptRegistry ::
  [TxId] ->
  BlockfrostClientT IO ScriptRegistry
queryScriptRegistry txIds = do
  Blockfrost.Genesis
    { _genesisNetworkMagic
    , _genesisSystemStart
    } <-
    queryGenesisParameters
  let networkId = toCardanoNetworkId _genesisNetworkMagic
  utxo <- queryUTxOByTxIn networkId candidates
  case newScriptRegistry utxo of
    Left e -> liftIO $ throwIO e
    Right sr -> pure sr
 where
  candidates = map (\txid -> TxIn txid (TxIx 0)) txIds

queryProtocolParameters :: MonadIO m => BlockfrostClientT m (PParams LedgerEra)
queryProtocolParameters = do
  pparams <- Blockfrost.getLatestEpochProtocolParams
  let minVersion = fromIntegral $ pparams ^. Blockfrost.protocolMinorVer
  maxVersion <- liftIO $ mkVersion $ pparams ^. Blockfrost.protocolMajorVer
  let results = do
        a0 <- boundRational (pparams ^. Blockfrost.a0)
        rho <- boundRational (pparams ^. Blockfrost.rho)
        tau <- boundRational (pparams ^. Blockfrost.tau)
        priceMemory <- boundRational @NonNegativeInterval (pparams ^. Blockfrost.priceMem)
        priceSteps <- boundRational @NonNegativeInterval (pparams ^. Blockfrost.priceStep)
        pvtMotionNoConfidence <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtMotionNoConfidence
        pvtCommitteeNormal <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtCommitteeNormal
        pvtCommitteeNoConfidence <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtCommitteeNoConfidence
        pvtHardForkInitiation <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtHardForkInitiation
        pvtPPSecurityGroup <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtppSecurityGroup
        dvtMotionNoConfidence <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtMotionNoConfidence
        dvtCommitteeNormal <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtCommitteeNormal
        dvtCommitteeNoConfidence <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtCommitteeNoConfidence
        dvtUpdateToConstitution <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtUpdateToConstitution
        dvtHardForkInitiation <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtHardForkInitiation
        dvtPPNetworkGroup <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPNetworkGroup
        dvtPPEconomicGroup <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPEconomicGroup
        dvtPPTechnicalGroup <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPTechnicalGroup
        dvtPPGovGroup <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPGovGroup
        dvtTreasuryWithdrawal <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtTreasuryWithdrawal
        committeeMinSize <- pparams ^. Blockfrost.committeeMinSize
        committeeMaxTermLength <- pparams ^. Blockfrost.committeeMaxTermLength
        govActionLifetime <- pparams ^. Blockfrost.govActionLifetime
        govActionDeposit <- fromIntegral <$> pparams ^. Blockfrost.govActionDeposit
        drepDeposit <- fromIntegral <$> pparams ^. Blockfrost.drepDeposit
        drepActivity <- pparams ^. Blockfrost.drepActivity
        minFeeRefScriptCostPerByte <- boundRational @NonNegativeInterval =<< (pparams ^. Blockfrost.minFeeRefScriptCostPerByte)
        pure BlockfrostConversion{..}

  case results of
    Nothing -> liftIO $ throwIO $ DecodeError "Could not decode some values appropriately."
    Just BlockfrostConversion{..} ->
      pure $
        emptyPParams
          & ppMinFeeAL .~ fromIntegral (pparams ^. Blockfrost.minFeeA)
          & ppMinFeeBL .~ fromIntegral (pparams ^. Blockfrost.minFeeB)
          & ppMaxBBSizeL .~ fromIntegral (pparams ^. Blockfrost.maxBlockSize)
          & ppMaxTxSizeL .~ fromIntegral (pparams ^. Blockfrost.maxTxSize)
          & ppMaxBHSizeL .~ fromIntegral (pparams ^. Blockfrost.maxBlockHeaderSize)
          & ppKeyDepositL .~ fromIntegral (pparams ^. Blockfrost.keyDeposit)
          & ppPoolDepositL .~ fromIntegral (pparams ^. Blockfrost.poolDeposit)
          & ppEMaxL .~ EpochInterval (fromIntegral (pparams ^. Blockfrost.eMax))
          & ppNOptL .~ fromIntegral (pparams ^. Blockfrost.nOpt)
          & ppA0L .~ a0
          & ppRhoL .~ rho
          & ppTauL .~ tau
          & ppProtocolVersionL .~ ProtVer maxVersion minVersion
          & ppMinPoolCostL .~ fromIntegral (pparams ^. Blockfrost.minPoolCost)
          & ppCoinsPerUTxOByteL .~ CoinPerByte (fromIntegral (pparams ^. Blockfrost.coinsPerUtxoSize))
          & ppCostModelsL .~ convertCostModels (pparams ^. Blockfrost.costModelsRaw)
          & ppPricesL .~ Prices priceMemory priceSteps
          & ppMaxTxExUnitsL .~ ExUnits (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxTxExMem) (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxTxExSteps)
          & ppMaxBlockExUnitsL .~ ExUnits (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxBlockExMem) (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxBlockExSteps)
          & ppMaxValSizeL .~ fromIntegral (Blockfrost.unQuantity $ pparams ^. Blockfrost.maxValSize)
          & ppCollateralPercentageL .~ fromIntegral (pparams ^. Blockfrost.collateralPercent)
          & ppMaxCollateralInputsL .~ fromIntegral (pparams ^. Blockfrost.maxCollateralInputs)
          & ppPoolVotingThresholdsL .~ PoolVotingThresholds{pvtMotionNoConfidence, pvtCommitteeNormal, pvtCommitteeNoConfidence, pvtHardForkInitiation, pvtPPSecurityGroup}
          & ppDRepVotingThresholdsL .~ DRepVotingThresholds{dvtMotionNoConfidence, dvtCommitteeNormal, dvtCommitteeNoConfidence, dvtUpdateToConstitution, dvtHardForkInitiation, dvtPPNetworkGroup, dvtPPEconomicGroup, dvtPPTechnicalGroup, dvtPPGovGroup, dvtTreasuryWithdrawal}
          & ppCommitteeMinSizeL .~ fromIntegral (Blockfrost.unQuantity committeeMinSize)
          & ppCommitteeMaxTermLengthL .~ EpochInterval (fromIntegral $ Blockfrost.unQuantity committeeMaxTermLength)
          & ppGovActionLifetimeL .~ EpochInterval (fromIntegral $ Blockfrost.unQuantity govActionLifetime)
          & ppGovActionDepositL .~ govActionDeposit
          & ppDRepDepositL .~ fromIntegral drepDeposit
          & ppDRepActivityL .~ EpochInterval (fromIntegral $ Blockfrost.unQuantity drepActivity)
          & ppMinFeeRefScriptCostPerByteL .~ minFeeRefScriptCostPerByte
 where
  convertCostModels :: Blockfrost.CostModelsRaw -> CostModels
  convertCostModels costModels =
    let costModelsMap = Blockfrost.unCostModelsRaw costModels
     in foldMap
          ( (mempty <>)
              . ( \(scriptType, v) ->
                    case scriptTypeToPlutusVersion scriptType of
                      Nothing -> mempty
                      Just plutusScript ->
                        case mkCostModel plutusScript (fromIntegral <$> v) of
                          Left _ -> mempty
                          Right costModel -> mkCostModels $ Map.singleton plutusScript costModel
                )
          )
          (Map.toList costModelsMap)

-- ** Helpers

toCardanoUTxO :: NetworkId -> TxIn -> Blockfrost.Address -> Maybe Blockfrost.ScriptHash -> Maybe Blockfrost.DatumHash -> [Blockfrost.Amount] -> Maybe Blockfrost.InlineDatum -> BlockfrostClientT IO (UTxO' (TxOut ctx))
toCardanoUTxO networkId txIn address scriptHash datumHash amount inlineDatum = do
  let addrTxt = Blockfrost.unAddress address
  let datumHash' = Blockfrost.unDatumHash <$> datumHash
  let inlineDatum' = Blockfrost._scriptDatumCborCbor . Blockfrost.unInlineDatum <$> inlineDatum
  val <- toCardanoValue amount
  plutusScript <- maybe (pure Nothing) (queryScript . Blockfrost.unScriptHash) scriptHash
  o <- toCardanoTxOut networkId addrTxt val datumHash' inlineDatum' plutusScript
  pure $ UTxO.singleton txIn o

scriptTypeToPlutusVersion :: Blockfrost.ScriptType -> Maybe Language
scriptTypeToPlutusVersion = \case
  Blockfrost.PlutusV1 -> Just PlutusV1
  Blockfrost.PlutusV2 -> Just PlutusV2
  Blockfrost.PlutusV3 -> Just PlutusV3
  Blockfrost.Timelock -> Nothing

toCardanoPoolId :: Blockfrost.PoolId -> Hash StakePoolKey
toCardanoPoolId (Blockfrost.PoolId textPoolId) =
  case deserialiseFromRawBytesHex (encodeUtf8 textPoolId) of
    Left err -> error (show err)
    Right pool -> pool

toCardanoTxIn :: Text -> Integer -> TxIn
toCardanoTxIn txHash i =
  case deserialiseFromRawBytesHex (encodeUtf8 txHash) of
    Left err -> error (show err)
    Right txid -> TxIn txid (TxIx (fromIntegral i))

toCardanoTxOut :: NetworkId -> Text -> Value -> Maybe Text -> Maybe Text -> Maybe PlutusScript -> BlockfrostClientT IO (TxOut ctx)
toCardanoTxOut networkId addrTxt val mDatumHash mInlineDatum plutusScript = do
  let datum =
        case mInlineDatum of
          Nothing ->
            case mDatumHash of
              Nothing -> TxOutDatumNone
              Just datumHash -> TxOutDatumHash (fromString $ T.unpack datumHash)
          Just cborDatum ->
            case deserialiseFromCBOR (proxyToAsType (Proxy @HashableScriptData)) (encodeUtf8 cborDatum) of
              Left _ -> TxOutDatumNone
              Right hashableScriptData -> TxOutDatumInline hashableScriptData
  case plutusScript of
    Nothing -> do
      case toCardanoAddress addrTxt of
        Nothing -> liftIO $ throwIO $ BlockfrostError $ FailedToDecodeAddress addrTxt
        Just addr -> pure $ TxOut addr val datum ReferenceScriptNone
    Just script -> pure $ TxOut (scriptAddr script) val datum (mkScriptRef script)
 where
  scriptAddr script =
    makeShelleyAddressInEra
      shelleyBasedEra
      networkId
      (PaymentCredentialByScript $ hashScript $ PlutusScript script)
      NoStakeAddress

toCardanoPolicyIdAndAssetName :: Text -> BlockfrostClientT IO (PolicyId, AssetName)
toCardanoPolicyIdAndAssetName pid = do
  Blockfrost.AssetDetails{_assetDetailsPolicyId, _assetDetailsAssetName} <- Blockfrost.getAssetDetails (Blockfrost.mkAssetId pid)
  case deserialiseFromRawBytesHex (encodeUtf8 $ Blockfrost.unPolicyId _assetDetailsPolicyId) of
    Left err -> liftIO $ throwIO $ BlockfrostError $ DeserialiseError (show err)
    Right p ->
      case _assetDetailsAssetName of
        Nothing -> liftIO $ throwIO $ BlockfrostError AssetNameMissing
        Just assetName ->
          case deserialiseFromRawBytesHex (encodeUtf8 assetName) of
            Left err -> liftIO $ throwIO $ BlockfrostError $ DeserialiseError (show err)
            Right asset -> pure (p, asset)

toCardanoValue :: [Blockfrost.Amount] -> BlockfrostClientT IO Value
toCardanoValue = foldMapM convertAmount
 where
  convertAmount :: Blockfrost.Amount -> BlockfrostClientT IO Value
  convertAmount (Blockfrost.AdaAmount lovelaces) =
    pure $
      fromList
        [
          ( AdaAssetId
          , Quantity (toInteger lovelaces)
          )
        ]
  convertAmount (Blockfrost.AssetAmount money) = do
    let currency = Money.someDiscreteCurrency money
    (cardanoPolicyId, assetName) <- toCardanoPolicyIdAndAssetName currency
    pure $
      fromList
        [
          ( AssetId
              cardanoPolicyId
              assetName
          , Quantity (Money.someDiscreteAmount money)
          )
        ]

toCardanoAddress :: Text -> Maybe AddressInEra
toCardanoAddress addrTxt =
  ShelleyAddressInEra <$> deserialiseAddress (AsAddress AsShelleyAddr) addrTxt

toCardanoNetworkId :: Integer -> NetworkId
toCardanoNetworkId magic =
  if fromIntegral magic == unProtocolMagicId mainnetProtocolMagicId
    then Mainnet
    else Testnet (NetworkMagic (fromInteger magic))

data BlockfrostConversion
  = BlockfrostConversion
  { a0 :: NonNegativeInterval
  , rho :: UnitInterval
  , tau :: UnitInterval
  , priceMemory :: NonNegativeInterval
  , priceSteps :: NonNegativeInterval
  , pvtMotionNoConfidence :: UnitInterval
  , pvtCommitteeNormal :: UnitInterval
  , pvtCommitteeNoConfidence :: UnitInterval
  , pvtHardForkInitiation :: UnitInterval
  , pvtPPSecurityGroup :: UnitInterval
  , dvtMotionNoConfidence :: UnitInterval
  , dvtCommitteeNormal :: UnitInterval
  , dvtCommitteeNoConfidence :: UnitInterval
  , dvtUpdateToConstitution :: UnitInterval
  , dvtHardForkInitiation :: UnitInterval
  , dvtPPNetworkGroup :: UnitInterval
  , dvtPPEconomicGroup :: UnitInterval
  , dvtPPTechnicalGroup :: UnitInterval
  , dvtPPGovGroup :: UnitInterval
  , dvtTreasuryWithdrawal :: UnitInterval
  , committeeMinSize :: Blockfrost.Quantity
  , committeeMaxTermLength :: Blockfrost.Quantity
  , govActionLifetime :: Blockfrost.Quantity
  , govActionDeposit :: Coin
  , drepDeposit :: Integer
  , drepActivity :: Blockfrost.Quantity
  , minFeeRefScriptCostPerByte :: NonNegativeInterval
  }

toCardanoGenesisParameters :: Blockfrost.Genesis -> GenesisParameters ShelleyEra
toCardanoGenesisParameters bfGenesis =
  GenesisParameters
    { protocolParamSlotsPerKESPeriod = fromIntegral _genesisSlotsPerKesPeriod
    , protocolParamUpdateQuorum = fromIntegral _genesisUpdateQuorum
    , protocolParamMaxLovelaceSupply = fromIntegral _genesisMaxLovelaceSupply
    , protocolParamSecurity = unsafeNonZero $ fromIntegral _genesisSecurityParam
    , protocolParamActiveSlotsCoefficient = _genesisActiveSlotsCoefficient
    , protocolParamSystemStart = posixSecondsToUTCTime _genesisSystemStart
    , protocolParamNetworkId = fromNetworkMagic $ NetworkMagic $ fromIntegral _genesisNetworkMagic
    , protocolParamMaxKESEvolutions = fromIntegral _genesisMaxKesEvolutions
    , protocolParamEpochLength = EpochSize $ fromIntegral _genesisEpochLength
    , protocolParamSlotLength = fromIntegral _genesisSlotLength
    , protocolInitialUpdateableProtocolParameters = def
    }
 where
  Blockfrost.Genesis
    { _genesisActiveSlotsCoefficient
    , _genesisUpdateQuorum
    , _genesisMaxLovelaceSupply
    , _genesisNetworkMagic
    , _genesisEpochLength
    , _genesisSystemStart
    , _genesisSlotsPerKesPeriod
    , _genesisSlotLength
    , _genesisMaxKesEvolutions
    , _genesisSecurityParam
    } = bfGenesis

submitTransaction :: MonadIO m => Tx -> BlockfrostClientT m Blockfrost.TxHash
submitTransaction tx = Blockfrost.submitTx $ Blockfrost.CBORString $ fromStrict $ serialiseToCBOR tx

----------------
-- Queries --
----------------

-- NOTE: Is this value good enough for all cardano networks?
queryTimeout :: Int
queryTimeout = 10

retryTimeout :: Int
retryTimeout = 300

queryEraHistory :: BlockfrostClientT IO EraHistory
queryEraHistory = do
  eras' <- Blockfrost.getNetworkEras
  let eras = filter isEmptyEra eras'
  let summary = mkEra <$> eras
  case nonEmptyFromList summary of
    Nothing ->
      liftIO $ throwIO $ BlockfrostError FailedEraHistory
    Just s -> pure $ EraHistory (mkInterpreter $ Summary s)
 where
  mkBound Blockfrost.NetworkEraBound{_boundEpoch, _boundSlot, _boundTime} =
    Bound
      { boundTime = RelativeTime _boundTime
      , boundSlot = SlotNo $ fromIntegral _boundSlot
      , boundEpoch = EpochNo $ fromIntegral _boundEpoch
      }
  mkEraParams Blockfrost.NetworkEraParameters{_parametersEpochLength, _parametersSlotLength, _parametersSafeZone} =
    EraParams
      { eraEpochSize = EpochSize $ fromIntegral _parametersEpochLength
      , eraSlotLength = mkSlotLength _parametersSlotLength
      , eraSafeZone = StandardSafeZone _parametersSafeZone
      , eraGenesisWin = GenesisWindow _parametersSafeZone
      }
  mkEra Blockfrost.NetworkEraSummary{_networkEraStart, _networkEraEnd, _networkEraParameters} =
    EraSummary
      { eraStart = mkBound _networkEraStart
      , eraEnd = EraEnd $ mkBound _networkEraEnd
      , eraParams = mkEraParams _networkEraParameters
      }
  isEmptyEra
    Blockfrost.NetworkEraSummary
      { _networkEraStart = Blockfrost.NetworkEraBound{_boundTime = boundStart}
      , _networkEraEnd = Blockfrost.NetworkEraBound{_boundTime = boundEnd}
      , _networkEraParameters
      } = boundStart /= 0 && boundEnd /= 0

-- | Query the Blockfrost API to get the 'UTxO' for 'TxIn' and convert to cardano 'UTxO'.
-- FIXME: make blockfrost wait times configurable.
queryUTxOByTxIn :: NetworkId -> [TxIn] -> BlockfrostClientT IO UTxO
queryUTxOByTxIn networkId = foldMapM (\(TxIn txid _) -> go retryTimeout (serialiseToRawBytesHexText txid))
 where
  go 0 txHash = liftIO $ throwIO $ BlockfrostError $ FailedUTxOForHash txHash
  go n txHash = do
    res <- Blockfrost.tryError $ Blockfrost.getTxUtxos (Blockfrost.TxHash txHash)
    case res of
      Left _e -> liftIO (threadDelay 1) >> go (n - 1) txHash
      Right Blockfrost.TransactionUtxos{_transactionUtxosInputs, _transactionUtxosOutputs} ->
        foldMapM
          ( \Blockfrost.UtxoOutput{_utxoOutputOutputIndex, _utxoOutputAddress, _utxoOutputAmount, _utxoOutputDataHash, _utxoOutputInlineDatum, _utxoOutputReferenceScriptHash} ->
              let txIn = toCardanoTxIn txHash _utxoOutputOutputIndex
               in toCardanoUTxO networkId txIn _utxoOutputAddress _utxoOutputReferenceScriptHash _utxoOutputDataHash _utxoOutputAmount _utxoOutputInlineDatum
          )
          _transactionUtxosOutputs

queryScript :: Text -> BlockfrostClientT IO (Maybe PlutusScript)
queryScript scriptHashTxt = do
  Blockfrost.ScriptCBOR{_scriptCborCbor} <- Blockfrost.getScriptCBOR $ Blockfrost.ScriptHash scriptHashTxt
  case _scriptCborCbor of
    Nothing -> pure Nothing
    Just fullScriptCBOR ->
      case decodeBase16 fullScriptCBOR :: Either String ByteString of
        Left _ -> pure Nothing
        Right bytes ->
          case deserialiseFromCBOR (proxyToAsType (Proxy @PlutusScript)) bytes of
            Left _ -> pure Nothing
            Right plutusScript -> pure $ Just plutusScript

-- | Query the Blockfrost API for address UTxO and convert to cardano 'UTxO'.
-- NOTE: We accept the address list here to be compatible with cardano-api but in
-- fact this is a single address query always.
queryUTxO :: NetworkId -> [Address ShelleyAddr] -> BlockfrostClientT IO UTxO
queryUTxO networkId addresses = do
  -- NOTE: We can't know at the time of doing a query if the information on specific address UTxO is _fresh_ or not
  -- so we try to wait for sufficient period of time and hope for best.
  liftIO $ threadDelay $ fromIntegral queryTimeout
  let address = List.head addresses
  let address' = Blockfrost.Address . serialiseAddress $ List.head addresses
  utxoWithAddresses <-
    Blockfrost.getAddressUtxos address'
      `catchError` \err ->
        if "BlockfrostNotFound" == T.pack (show err)
          then liftIO (throwIO (BlockfrostError (NoUTxOFound address)))
          else throwError err

  foldMapM
    ( \Blockfrost.AddressUtxo
        { Blockfrost._addressUtxoAddress
        , Blockfrost._addressUtxoTxHash = Blockfrost.TxHash{unTxHash}
        , Blockfrost._addressUtxoOutputIndex
        , Blockfrost._addressUtxoAmount
        , Blockfrost._addressUtxoBlock
        , Blockfrost._addressUtxoDataHash
        , Blockfrost._addressUtxoInlineDatum
        , Blockfrost._addressUtxoReferenceScriptHash
        } ->
          let txin = toCardanoTxIn unTxHash _addressUtxoOutputIndex
           in toCardanoUTxO networkId txin _addressUtxoAddress _addressUtxoReferenceScriptHash _addressUtxoDataHash _addressUtxoAmount _addressUtxoInlineDatum
    )
    utxoWithAddresses

queryUTxOFor :: VerificationKey PaymentKey -> BlockfrostClientT IO UTxO
queryUTxOFor vk = do
  Blockfrost.Genesis
    { _genesisNetworkMagic = networkMagic
    } <-
    queryGenesisParameters
  let networkId = toCardanoNetworkId networkMagic
  case mkVkAddress networkId vk of
    ShelleyAddressInEra addr ->
      queryUTxO networkId [addr]
    ByronAddressInEra{} ->
      liftIO $ throwIO $ BlockfrostError ByronAddressNotSupported

-- | Query the Blockfrost API for 'Genesis'
queryGenesisParameters :: BlockfrostClientT IO Blockfrost.Genesis
queryGenesisParameters = Blockfrost.getLedgerGenesis

querySystemStart :: BlockfrostClientT IO SystemStart
querySystemStart = do
  Blockfrost.Genesis{_genesisSystemStart} <- queryGenesisParameters
  pure $ SystemStart $ posixSecondsToUTCTime _genesisSystemStart

-- | Query the Blockfrost API for 'Genesis' and convert to cardano 'ChainPoint'.
queryTip :: BlockfrostClientT IO ChainPoint
queryTip = do
  Blockfrost.Block
    { _blockHeight
    , _blockHash
    , _blockSlot
    } <-
    Blockfrost.getLatestBlock
  let slotAndBlockNumber = do
        blockSlot <- _blockSlot
        blockNumber <- _blockHeight
        pure (blockSlot, blockNumber)
  case slotAndBlockNumber of
    Nothing -> pure $ chainTipToChainPoint ChainTipAtGenesis
    Just (blockSlot, blockNo) -> do
      let Blockfrost.BlockHash blockHash = _blockHash
      pure $
        chainTipToChainPoint $
          ChainTip
            (SlotNo $ fromIntegral $ Blockfrost.unSlot blockSlot)
            (fromString $ T.unpack blockHash)
            (BlockNo $ fromIntegral blockNo)

queryStakePools ::
  BlockfrostClientT IO (Set PoolId)
queryStakePools = do
  stakePools' <- Blockfrost.listPools
  pure $ Set.fromList (toCardanoPoolId <$> stakePools')

awaitTransaction :: Tx -> VerificationKey PaymentKey -> BlockfrostClientT IO UTxO
awaitTransaction tx vk = do
  Blockfrost.Genesis{_genesisNetworkMagic} <- queryGenesisParameters
  let networkId = toCardanoNetworkId _genesisNetworkMagic
  awaitUTxO networkId [makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress] (getTxId $ getTxBody tx) retryTimeout

-- | Await for specific UTxO at address - the one that is produced by the given 'TxId'.
awaitUTxO ::
  -- | Network id
  NetworkId ->
  -- | Address we are interested in
  [Address ShelleyAddr] ->
  -- | Last transaction ID to await
  TxId ->
  -- | Number of seconds to wait
  Int ->
  BlockfrostClientT IO UTxO
awaitUTxO networkId addresses txid i = do
  go i
 where
  go 0 = liftIO $ throwIO $ BlockfrostError (TimeoutOnUTxO txid)
  go n = do
    utxo <- Blockfrost.tryError $ queryUTxO networkId addresses
    case utxo of
      Left _e -> liftIO (threadDelay 1) >> go (n - 1)
      Right utxo' ->
        let wantedUTxO = UTxO.fromList $ List.filter (\(TxIn txid' _, _) -> txid' == txid) (UTxO.toList utxo')
         in if UTxO.null wantedUTxO
              then liftIO (threadDelay 1) >> go (n - 1)
              else pure utxo'
