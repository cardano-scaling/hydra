{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  SubmitTransactionException,
  buildAddress,
  sign,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow (Handler (Handler), catches)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithMintingScript, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.ScriptRegistry (
  publishHydraScripts,
  queryScriptRegistry,
 )
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()
import Hydra.Options qualified as Options
import Hydra.Tx (balance, txId)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data FaucetException
  = FaucetHasNotEnoughFunds {faucetUTxO :: UTxO}
  | FaucetFailedToBuildTx {reason :: TxBodyErrorAutoBalance Era}
  | FaucetBlockfrostError {blockFrostError :: Text}
  deriving stock (Show)

instance Exception FaucetException

data FaucetLog
  = TraceResourceExhaustedHandled Text
  | ReturnedFunds {returnAmount :: Coin}
  | SubmitTxError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

delayBF :: (ChainBackend m, MonadDelay m) => m ()
delayBF = do
  delay <- Backend.getQueryDelay
  threadDelay $ realToFrac delay

seedFromFaucet ::
  ChainBackend m =>
  MonadIO m =>
  MonadDelay m =>
  MonadCatch m =>
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer m FaucetLog ->
  m UTxO
seedFromFaucet receivingVerificationKey val tracer = do
  delayBF
  seedFromFaucetWithMinting receivingVerificationKey val tracer Nothing

-- | Create a specially marked "seed" UTXO containing requested 'Value' by
-- redeeming funds available to the well-known faucet.
seedFromFaucetWithMinting ::
  ChainBackend m =>
  MonadIO m =>
  MonadDelay m =>
  MonadCatch m =>
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer m FaucetLog ->
  Maybe PlutusScript ->
  m UTxO
seedFromFaucetWithMinting receivingVerificationKey val tracer mintingScript = do
  (faucetVk, faucetSk) <- liftIO $ keysFor Faucet
  networkId <- Backend.queryNetworkId
  seedTx <- retryOnExceptions tracer $ submitSeedTx faucetVk faucetSk networkId
  producedUTxO <- Backend.awaitTransaction seedTx receivingVerificationKey
  pure $ UTxO.filter (== toCtxUTxOTxOut (theOutput networkId)) producedUTxO
 where
  submitSeedTx faucetVk faucetSk networkId = do
    faucetUTxO <- findFaucetUTxO networkId (selectLovelace val)
    let changeAddress = mkVkAddress networkId faucetVk

    buildTransactionWithMintingScript changeAddress faucetUTxO (toList $ UTxO.inputSet faucetUTxO) [theOutput networkId] mintingScript >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> do
        let signedTx = sign faucetSk $ getTxBody tx
        Backend.submitTransaction signedTx
        pure signedTx

  receivingAddress = buildAddress receivingVerificationKey

  theOutput networkId =
    TxOut
      (shelleyAddressInEra shelleyBasedEra (receivingAddress networkId))
      val
      TxOutDatumNone
      ReferenceScriptNone

findFaucetUTxO :: ChainBackend m => MonadThrow m => MonadIO m => NetworkId -> Coin -> m UTxO
findFaucetUTxO networkId lovelace = do
  (faucetVk, _) <- liftIO $ keysFor Faucet
  faucetUTxO <- Backend.queryUTxO [buildAddress faucetVk networkId]
  let foundUTxO = UTxO.filter (\o -> (selectLovelace . txOutValue) o >= lovelace) faucetUTxO
  when (UTxO.null foundUTxO) $
    throwIO $
      FaucetHasNotEnoughFunds{faucetUTxO}
  pure foundUTxO

seedFromFaucetBlockfrost ::
  Options.BlockfrostOptions ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Blockfrost.BlockfrostClientT IO UTxO
seedFromFaucetBlockfrost options receivingVerificationKey lovelace = do
  (faucetVk, faucetSk) <- liftIO $ keysFor Faucet

  Blockfrost.Genesis
    { Blockfrost._genesisNetworkMagic = networkMagic
    , Blockfrost._genesisSystemStart = systemStart'
    } <-
    Blockfrost.queryGenesisParameters
  pparams <- Blockfrost.queryProtocolParameters
  let networkId = Blockfrost.toCardanoNetworkId networkMagic
  let changeAddress = buildAddress faucetVk networkId
  let receivingAddress = buildAddress receivingVerificationKey networkId
  let theOutput =
        TxOut
          (shelleyAddressInEra shelleyBasedEra receivingAddress)
          (lovelaceToValue lovelace)
          TxOutDatumNone
          ReferenceScriptNone
  stakePools' <- Blockfrost.listPools
  let stakePools = Set.fromList (Blockfrost.toCardanoPoolId <$> stakePools')
  let systemStart = SystemStart $ posixSecondsToUTCTime systemStart'
  eraHistory <- Blockfrost.queryEraHistory
  faucetUTxO <- Blockfrost.queryUTxO options networkId [changeAddress]
  foundUTxO <- findUTxO faucetUTxO lovelace
  case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId faucetVk) foundUTxO [] [theOutput] Nothing of
    Left e -> liftIO $ throwIO $ FaucetFailedToBuildTx{reason = e}
    Right tx -> do
      let signedTx = signTx faucetSk tx
      eResult <- Blockfrost.tryError $ Blockfrost.submitTransaction signedTx
      case eResult of
        Left err -> liftIO $ throwIO $ FaucetBlockfrostError{blockFrostError = show err}
        Right _ -> do
          void $ Blockfrost.awaitUTxO networkId [changeAddress] (Hydra.Tx.txId signedTx) options
          Blockfrost.awaitUTxO networkId [receivingAddress] (Hydra.Tx.txId signedTx) options

findUTxO :: MonadIO m => UTxO.UTxO Era -> Lovelace -> m (UTxO.UTxO Era)
findUTxO utxo lovelace' = do
  let foundUTxO = UTxO.find (\o -> (selectLovelace . txOutValue) o >= lovelace') utxo
  when (isNothing foundUTxO) $
    liftIO $
      throwIO $
        FaucetHasNotEnoughFunds{faucetUTxO = utxo}
  pure $ maybe mempty (uncurry UTxO.singleton) foundUTxO

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  ChainBackend m =>
  MonadIO m =>
  MonadDelay m =>
  MonadCatch m =>
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Tracer m FaucetLog ->
  m ()
seedFromFaucet_ vk ll tracer =
  void $ seedFromFaucet vk (lovelaceToValue ll) tracer

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  ChainBackend m =>
  MonadIO m =>
  MonadDelay m =>
  MonadCatch m =>
  Tracer m FaucetLog ->
  Actor ->
  m ()
returnFundsToFaucet tracer sender = do
  delayBF
  senderKeys <- liftIO $ keysFor sender
  void $ returnFundsToFaucet' tracer (snd senderKeys)

returnFundsToFaucet' ::
  ChainBackend m =>
  MonadCatch m =>
  MonadIO m =>
  MonadDelay m =>
  Tracer m FaucetLog ->
  SigningKey PaymentKey ->
  m Coin
returnFundsToFaucet' tracer senderSk = do
  (faucetVk, _) <- liftIO $ keysFor Faucet
  networkId <- Backend.queryNetworkId
  let faucetAddress = mkVkAddress networkId faucetVk
  let senderVk = getVerificationKey senderSk
  utxo <- Backend.queryUTxOFor QueryTip senderVk
  returnAmount <-
    if UTxO.null utxo
      then pure 0
      else retryOnExceptions tracer $ do
        let utxoValue = balance @Tx utxo
        let allLovelace = selectLovelace utxoValue
        tx <- sign senderSk <$> buildTxBody utxo faucetAddress
        Backend.submitTransaction tx
        void $ Backend.awaitTransaction tx faucetVk
        pure allLovelace
  traceWith tracer $ ReturnedFunds{returnAmount}
  pure returnAmount
 where
  buildTxBody utxo faucetAddress =
    -- Here we specify no outputs in the transaction so that a change output with the
    -- entire value is created and paid to the faucet address.
    buildTransaction faucetAddress utxo [] [] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> pure $ getTxBody tx

-- Use the Faucet utxo to create the output at specified address
createOutputAtAddress ::
  ChainBackend m =>
  MonadThrow m =>
  MonadIO m =>
  NetworkId ->
  AddressInEra ->
  TxOutDatum CtxTx ->
  Value ->
  m (TxIn, TxOut CtxUTxO)
createOutputAtAddress networkId atAddress datum val = do
  (faucetVk, faucetSk) <- liftIO $ keysFor Faucet
  utxo <- findFaucetUTxO networkId (selectLovelace val)
  let collateralTxIns = mempty
  let output = TxOut atAddress val datum ReferenceScriptNone
  buildTransaction (mkVkAddress networkId faucetVk) utxo collateralTxIns [output] >>= \case
    Left e ->
      liftIO $ throwErrorAsException e
    Right x -> do
      let body = getTxBody x
      let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey faucetSk)] body
      Backend.submitTransaction tx
      newUtxo <- Backend.awaitTransaction tx faucetVk
      case UTxO.find (\out -> txOutAddress out == atAddress) newUtxo of
        Nothing -> failure $ "Could not find script output: " <> decodeUtf8 (encodePretty newUtxo)
        Just u -> pure u

-- | Try to submit tx and retry when some caught exception/s take place.
retryOnExceptions :: (MonadCatch m, MonadDelay m, ChainBackend m) => Tracer m FaucetLog -> m a -> m a
retryOnExceptions tracer action =
  action
    `catches` [ Handler $ \(ex :: SubmitTransactionException) -> do
                  delayBF backend
                  traceWith tracer $
                    SubmitTxError $
                      show ex
                  retryOnExceptions tracer backend action
              , Handler $ \(ex :: IOException) -> do
                  unless (isResourceExhausted ex) $
                    throwIO ex
                  traceWith tracer $
                    TraceResourceExhaustedHandled $
                      "Expected exception raised from seedFromFaucet: " <> show ex
                  delayBF
                  retryOnExceptions tracer action
              ]
 where
  isResourceExhausted ex = case ioe_type ex of
    ResourceExhausted -> True
    _other -> False

-- | Publish current Hydra scripts as scripts outputs for later referencing them.
--
-- The key of the given Actor is used to pay for fees in required transactions,
-- it is expected to have sufficient funds.
publishHydraScriptsAs ::
   ChainBackend m => MonadIO m => MonadDelay m
  => MonadCatch m => Actor -> m [TxId]
publishHydraScriptsAs actor = do
  (_, sk) <- liftIO $ keysFor actor
  txids <- publishHydraScripts sk
  delayBF
  pure txids

-- | Like 'publishHydraScriptsAs', but caches the resulting 'TxId's to a file
-- in the given directory. On subsequent calls, the cached 'TxId's are validated
-- against the chain (using 'queryScriptRegistry') and reused if still valid.
-- This avoids re-publishing identical scripts on every test run, saving funds
-- and time especially on public testnets.
publishOrReuseHydraScripts :: ChainBackend backend => backend -> Actor -> FilePath -> IO [TxId]
publishOrReuseHydraScripts backend actor cacheDir = do
  let cacheFile = cacheDir </> ".hydra-scripts-tx-ids"
  readCachedTxIds cacheFile >>= \case
    Just txIds -> do
      result <- try $ queryScriptRegistry backend txIds
      case result of
        Right _registry -> pure txIds
        Left (_ :: SomeException) -> publishAndCache cacheFile
    Nothing -> publishAndCache cacheFile
 where
  readCachedTxIds :: FilePath -> IO (Maybe [TxId])
  readCachedTxIds path = do
    exists <- doesFileExist path
    if exists
      then either (const Nothing) Just <$> Aeson.eitherDecodeFileStrict path
      else pure Nothing

  publishAndCache :: FilePath -> IO [TxId]
  publishAndCache path = do
    txIds <- publishHydraScriptsAs backend actor
    Aeson.encodeFile path txIds
    pure txIds
