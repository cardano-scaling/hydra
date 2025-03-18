{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Blockfrost.Client where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX
import Hydra.Cardano.Api hiding (fromNetworkMagic)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Hash.Class (hashFromTextAsHex)
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.BaseTypes (EpochInterval (..), NonNegativeInterval, UnitInterval, boundRational)
import Cardano.Ledger.Binary.Version (mkVersion)
import Cardano.Ledger.Conway.Core (DRepVotingThresholds (..), PoolVotingThresholds (..), ppCommitteeMaxTermLengthL, ppCommitteeMinSizeL, ppDRepActivityL, ppDRepDepositL, ppDRepVotingThresholdsL, ppGovActionDepositL, ppGovActionLifetimeL, ppPoolVotingThresholdsL)
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Plutus (ExUnits (..), Language (..), Prices (..))
import Cardano.Ledger.Plutus.CostModels (CostModels, mkCostModel, mkCostModels)
import Cardano.Ledger.Shelley.API (ProtVer (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (slotLengthFromSec)
import Control.Lens ((.~), (^.))
import Data.Set qualified as Set
import Hydra.Cardano.Api.Prelude (StakePoolKey)
import Hydra.Contract.Head qualified as Head
import Hydra.Ledger.Cardano.Evaluate (epochSize)
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Money qualified

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
    Blockfrost.Genesis
      { _genesisNetworkMagic = networkMagic
      , _genesisSystemStart = systemStart
      , _genesisSlotLength = slotLength
      } <-
      Blockfrost.getLedgerGenesis
    pparams <- liftIO fromBlockfrostPParams
    let epoch = fixedEpochInfo epochSize (slotLengthFromSec slotLength)
    let address = Blockfrost.Address (vkAddress networkMagic)
    let networkId = fromNetworkMagic networkMagic
    let changeAddress = mkVkAddress networkId vk
    stakePools <- Blockfrost.listPools
    forM scripts $ \script -> do
      liftIO $ threadDelay 2
      utxo <- Blockfrost.getAddressUtxos address
      liftIO $ threadDelay 2
      liftIO $
        buildTx pparams (LedgerEpochInfo epoch) networkId systemStart stakePools script changeAddress utxo
          >>= \case
            Left err -> throwErrorAsException err
            Right rawTx -> do
              let body = getTxBody rawTx
                  tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
                  txByteString = serialiseToCBOR tx
                  txCborString = Blockfrost.CBORString $ fromStrict txByteString
              let txEnvelope = toJSON $ serialiseToTextEnvelope Nothing tx
              writeFileBS (show (getTxId (getTxBody tx)) <> ".cbor") txByteString
              void $ writeFileJSON (show (getTxId (getTxBody tx)) <> ".envelope.cbor") txEnvelope
              txHash <- Blockfrost.submitTx txCborString
              let numberOfTries :: Int = 100
              Blockfrost.Transaction{_transactionHash} <- awaitTransaction numberOfTries txHash

              case hashFromTextAsHex _transactionHash of
                Nothing -> liftIO $ throwIO $ BlockfrostError "Could not decode transaction hash."
                Just txHash' -> pure $ TxId txHash'
 where
  awaitTransaction :: Int -> Blockfrost.TxHash -> IO Blockfrost.Transaction
  awaitTransaction n expectedTxHash =
    if n <= 0
      then throwIO $ BlockfrostError "Could not find transaction."
      else do
        Blockfrost.getTx expectedTxHash
          `catch` ( \(_ :: SomeException) -> liftIO $ do
                      threadDelay 2
                      awaitTransaction (n - 1) expectedTxHash
                  )

  scripts = [initialValidatorScript, commitValidatorScript, Head.validatorScript]

  vk = getVerificationKey sk

  vkAddress networkMagic = textAddrOf (fromNetworkMagic networkMagic) vk

fromBlockfrostPParams :: IO (PParams LedgerEra)
fromBlockfrostPParams = do
  pparams <- Blockfrost.getLatestEpochProtocolParams
  minVersion <- mkVersion $ pparams ^. Blockfrost.protocolMinorVer
  let maxVersion = fromIntegral $ pparams ^. Blockfrost.protocolMajorVer
  let results = do
        a0' <- boundRational (pparams ^. Blockfrost.a0)
        rho' <- boundRational (pparams ^. Blockfrost.rho)
        tau' <- boundRational (pparams ^. Blockfrost.tau)
        priceMemory' <- boundRational @NonNegativeInterval (pparams ^. Blockfrost.priceMem)
        priceSteps' <- boundRational @NonNegativeInterval (pparams ^. Blockfrost.priceStep)
        pvtMotionNoConfidence' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtMotionNoConfidence
        pvtCommitteeNormal' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtCommitteeNormal
        pvtCommitteeNoConfidence' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtCommitteeNoConfidence
        pvtHardForkInitiation' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtHardForkInitiation
        pvtPPSecurityGroup' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.pvtppSecurityGroup
        dvtMotionNoConfidence' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtMotionNoConfidence
        dvtCommitteeNormal' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtCommitteeNormal
        dvtCommitteeNoConfidence' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtCommitteeNoConfidence
        dvtUpdateToConstitution' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtUpdateToConstitution
        dvtHardForkInitiation' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtHardForkInitiation
        dvtPPNetworkGroup' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPNetworkGroup
        dvtPPEconomicGroup' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPEconomicGroup
        dvtPPTechnicalGroup' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPTechnicalGroup
        dvtPPGovGroup' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtPPGovGroup
        dvtTreasuryWithdrawal' <- boundRational @UnitInterval =<< pparams ^. Blockfrost.dvtTreasuryWithdrawal
        committeeMinSize' <- pparams ^. Blockfrost.committeeMinSize
        committeeMaxTermLength' <- pparams ^. Blockfrost.committeeMaxTermLength
        govActionLifetime' <- pparams ^. Blockfrost.govActionLifetime
        govActionDeposit' <- pparams ^. Blockfrost.govActionDeposit
        drepDeposit' <- pparams ^. Blockfrost.drepDeposit
        drepActivity' <- pparams ^. Blockfrost.drepActivity
        minFeeRefScriptCostPerByte' <- boundRational @NonNegativeInterval =<< (pparams ^. Blockfrost.minFeeRefScriptCostPerByte)

        pure (a0', rho', tau', priceMemory', priceSteps', pvtMotionNoConfidence', pvtCommitteeNormal', pvtCommitteeNoConfidence', pvtHardForkInitiation', pvtPPSecurityGroup', dvtMotionNoConfidence', dvtCommitteeNormal', dvtCommitteeNoConfidence', dvtUpdateToConstitution', dvtHardForkInitiation', dvtPPNetworkGroup', dvtPPEconomicGroup', dvtPPTechnicalGroup', dvtPPGovGroup', dvtTreasuryWithdrawal', committeeMinSize', committeeMaxTermLength', govActionLifetime', govActionDeposit', drepDeposit', drepActivity', minFeeRefScriptCostPerByte')

  case results of
    Nothing -> liftIO $ throwIO $ DecodeError "Could not decode some values appropriately."
    Just (a0, rho, tau, priceMemory, priceSteps, pvtMotionNoConfidence, pvtCommitteeNormal, pvtCommitteeNoConfidence, pvtHardForkInitiation, pvtPPSecurityGroup, dvtMotionNoConfidence, dvtCommitteeNormal, dvtCommitteeNoConfidence, dvtUpdateToConstitution, dvtHardForkInitiation, dvtPPNetworkGroup, dvtPPEconomicGroup, dvtPPTechnicalGroup, dvtPPGovGroup, dvtTreasuryWithdrawal, committeeMinSize, committeeMaxTermLength, govActionLifetime, govActionDeposit, drepDeposit, drepActivity, minFeeRefScriptCostPerByte) ->
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
          & ppGovActionDepositL .~ fromIntegral govActionDeposit
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

scriptTypeToPlutusVersion :: Blockfrost.ScriptType -> Maybe Language
scriptTypeToPlutusVersion = \case
  Blockfrost.PlutusV1 -> Just PlutusV1
  Blockfrost.PlutusV2 -> Just PlutusV2
  Blockfrost.PlutusV3 -> Just PlutusV3
  Blockfrost.Timelock -> Nothing

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

  mkScriptTxOut =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone

  outputs = mkScriptTxOut <$> [mkScriptRef script]
  utxo' = toApiUTxO utxo changeAddress
  totalDeposit = sum (selectLovelace . txOutValue <$> outputs)
  utxoToSpend = maybe mempty UTxO.singleton $ UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo'
  systemStart = SystemStart $ posixSecondsToUTCTime posixTime
  collateral = mempty
  dummyFeeForBalancing = TxFeeExplicit 1500000
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
    let currency = Money.someDiscreteCurrency money
     in fromList
          [
            ( AssetId
                (toApiPolicyId currency)
                (toApiAssetName currency)
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

fromNetworkMagic :: Integer -> NetworkId
fromNetworkMagic = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))
