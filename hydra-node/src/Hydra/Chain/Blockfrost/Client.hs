{-# LANGUAGE RecordWildCards #-}

module Hydra.Chain.Blockfrost.Client where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX
import Hydra.Cardano.Api hiding (LedgerState, fromNetworkMagic)

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
import Cardano.Slotting.Time (mkSlotLength)
import Control.Lens ((.~), (^.))
import Data.Default (def)
import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Hydra.Cardano.Api.Prelude (StakePoolKey, fromNetworkMagic)
import Hydra.Chain.ScriptRegistry (buildScriptPublishingTxs)
import Hydra.Tx (txId)
import Money qualified
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras, StandardCrypto)
import Ouroboros.Consensus.HardFork.History (EraEnd (..), EraParams (..), EraSummary (..), SafeZone (..), Summary (..), initBound, mkInterpreter)

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
    genesis@Blockfrost.Genesis
      { _genesisNetworkMagic = networkMagic
      , _genesisSystemStart = systemStart'
      } <-
      queryGenesis
    pparams <- toCardanoPParams
    let address = Blockfrost.Address (vkAddress networkMagic)
    let networkId = toCardanoNetworkId networkMagic
    let changeAddress = mkVkAddress networkId vk
    stakePools' <- Blockfrost.listPools
    let stakePools = Set.fromList (toCardanoPoolId <$> stakePools')
    let systemStart = SystemStart $ posixSecondsToUTCTime systemStart'
    let eraHistory = mkEraHistory genesis
    utxo <- Blockfrost.getAddressUtxos address
    let cardanoUTxO = toCardanoUTxO utxo changeAddress

    txs <- liftIO $ buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools cardanoUTxO sk
    forM txs $ \(tx :: Tx) -> do
      void $ Blockfrost.submitTx $ Blockfrost.CBORString $ fromStrict $ serialiseToCBOR tx
      pure $ txId tx
 where
  vk = getVerificationKey sk

  vkAddress networkMagic = textAddrOf (toCardanoNetworkId networkMagic) vk

scriptTypeToPlutusVersion :: Blockfrost.ScriptType -> Maybe Language
scriptTypeToPlutusVersion = \case
  Blockfrost.PlutusV1 -> Just PlutusV1
  Blockfrost.PlutusV2 -> Just PlutusV2
  Blockfrost.PlutusV3 -> Just PlutusV3
  Blockfrost.Timelock -> Nothing

toCardanoPoolId :: Blockfrost.PoolId -> Hash StakePoolKey
toCardanoPoolId (Blockfrost.PoolId textPoolId) =
  case deserialiseFromRawBytesHex (AsHash AsStakePoolKey) (encodeUtf8 textPoolId) of
    Left err -> error (show err)
    Right pool -> pool

toCardanoUTxO :: [Blockfrost.AddressUtxo] -> AddressInEra -> UTxO' (TxOut CtxUTxO)
toCardanoUTxO utxos addr = UTxO.fromPairs (toEntry <$> utxos)
 where
  toEntry :: Blockfrost.AddressUtxo -> (TxIn, TxOut CtxUTxO)
  toEntry utxo = (toCardanoTxIn utxo, toCardanoTxOut utxo addr)

toCardanoTxIn :: Blockfrost.AddressUtxo -> TxIn
toCardanoTxIn Blockfrost.AddressUtxo{_addressUtxoTxHash = Blockfrost.TxHash{unTxHash}, _addressUtxoOutputIndex} =
  case deserialiseFromRawBytesHex AsTxId (encodeUtf8 unTxHash) of
    Left err -> error (show err)
    Right txid -> TxIn txid (TxIx (fromIntegral _addressUtxoOutputIndex))

-- REVIEW! TxOutDatumNone and ReferenceScriptNone
toCardanoTxOut :: Blockfrost.AddressUtxo -> AddressInEra -> TxOut CtxUTxO
toCardanoTxOut addrUTxO addr =
  TxOut addr (toCardanoValue _addressUtxoAmount) TxOutDatumNone ReferenceScriptNone
 where
  Blockfrost.AddressUtxo{_addressUtxoAmount, _addressUtxoDataHash, _addressUtxoInlineDatum, _addressUtxoReferenceScriptHash} = addrUTxO

toCardanoPolicyId :: Text -> PolicyId
toCardanoPolicyId pid =
  case deserialiseFromRawBytesHex AsPolicyId (encodeUtf8 pid) of
    Left err -> error (show err)
    Right p -> p

toCardanoAssetName :: Text -> AssetName
toCardanoAssetName = AssetName . encodeUtf8

toCardanoValue :: [Blockfrost.Amount] -> Value
toCardanoValue = foldMap convertAmount
 where
  convertAmount (Blockfrost.AdaAmount lovelaces) =
    fromList
      [
        ( AdaAssetId
        , Quantity (toInteger lovelaces)
        )
      ]
  convertAmount (Blockfrost.AssetAmount money) =
    let currency = Money.someDiscreteCurrency money
     in fromList
          [
            ( AssetId
                (toCardanoPolicyId currency)
                (toCardanoAssetName currency)
            , Quantity (Money.someDiscreteAmount money)
            )
          ]

-- ** Helpers

unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."

textAddrOf :: NetworkId -> VerificationKey PaymentKey -> Text
textAddrOf networkId vk = unwrapAddress (mkVkAddress @Era networkId vk)

toCardanoNetworkId :: Integer -> NetworkId
toCardanoNetworkId = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))

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

toCardanoPParams :: MonadIO m => BlockfrostClientT m (PParams LedgerEra)
toCardanoPParams = do
  pparams <- Blockfrost.getLatestEpochProtocolParams
  minVersion <- liftIO $ mkVersion $ pparams ^. Blockfrost.protocolMinorVer
  let maxVersion = fromIntegral $ pparams ^. Blockfrost.protocolMajorVer
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
          & ppProtocolVersionL .~ ProtVer minVersion maxVersion
          & ppMinPoolCostL .~ fromIntegral (pparams ^. Blockfrost.minPoolCost)
          & ppCoinsPerUTxOByteL .~ CoinPerByte (fromIntegral (pparams ^. Blockfrost.coinsPerUtxoSize))
          & ppCostModelsL .~ convertCostModels (pparams ^. Blockfrost.costModels)
          & ppPricesL .~ Prices priceMemory priceSteps
          & ppMaxTxExUnitsL .~ ExUnits (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxTxExSteps) (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxTxExMem)
          & ppMaxBlockExUnitsL .~ ExUnits (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxBlockExSteps) (fromIntegral $ Blockfrost.unQuantity $ pparams ^. Blockfrost.maxBlockExMem)
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
  convertCostModels :: Blockfrost.CostModels -> CostModels
  convertCostModels costModels =
    let costModelsMap = Blockfrost.unCostModels costModels
     in foldMap
          ( (mempty <>)
              . ( \(scriptType, v) ->
                    case scriptTypeToPlutusVersion scriptType of
                      Nothing -> mempty
                      Just plutusScript ->
                        case mkCostModel plutusScript (fromIntegral <$> Map.elems v) of
                          Left _ -> mempty
                          Right costModel -> mkCostModels $ Map.singleton plutusScript costModel
                )
          )
          (Map.toList costModelsMap)

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

mkEraHistory :: Blockfrost.Genesis -> EraHistory
mkEraHistory genesis = EraHistory (mkInterpreter summary)
 where
  Blockfrost.Genesis
    { _genesisNetworkMagic
    , _genesisSystemStart
    , _genesisSlotLength
    , _genesisEpochLength
    } = genesis

  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary . NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd = EraUnbounded
        , eraParams
        }

  eraParams =
    EraParams
      { eraEpochSize = EpochSize $ fromIntegral _genesisEpochLength
      , eraSlotLength = mkSlotLength $ fromIntegral _genesisSlotLength
      , eraSafeZone = UnsafeIndefiniteSafeZone
      , eraGenesisWin = GenesisWindow 1
      }

----------------
-- Wallet API --
----------------

-- | Query the Blockfrost API for address UTxO and convert to cardano 'UTxO'.
queryUTxO :: SigningKey PaymentKey -> NetworkId -> BlockfrostClientT IO UTxO
queryUTxO sk networkId = do
  let address = Blockfrost.Address vkAddress
  utxo <- Blockfrost.getAddressUtxos address
  let cardanoAddress = mkVkAddress networkId vk
  pure $ toCardanoUTxO utxo cardanoAddress
 where
  vk = getVerificationKey sk
  vkAddress = textAddrOf networkId vk

-- | Query the Blockfrost API for 'Genesis' and convert to cardano 'SystemStart'.
querySystemStart :: BlockfrostClientT IO SystemStart
querySystemStart = do
  Blockfrost.Genesis{_genesisSystemStart} <- Blockfrost.getLedgerGenesis
  pure $ SystemStart $ posixSecondsToUTCTime _genesisSystemStart

-- | Query the Blockfrost API for 'Genesis'
queryGenesis :: BlockfrostClientT IO Blockfrost.Genesis
queryGenesis = Blockfrost.getLedgerGenesis

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
