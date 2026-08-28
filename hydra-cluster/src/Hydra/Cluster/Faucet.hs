{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api hiding (getVerificationKey, signTx)
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  SubmitTransactionException,
  buildAddress,
  runBackend,
  sign,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow (Handler (Handler), catches)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Backend (ChainBackend (..), buildTransaction, buildTransactionWithMintingScript, buildTransactionWithPParams')
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()
import Hydra.Options (ChainBackendOptions (..))
import Hydra.Options qualified as Options
import Hydra.Tx (balance)
import Hydra.Tx.Crypto (getVerificationKey, signTx)
import Hydra.Tx.Secret (Secret, mkSecret, withSecret)
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

delayBF :: MonadDelay m => ChainBackendOptions -> m ()
delayBF opts = do
  let delay = case opts of
        Options.Blockfrost{} -> 30 :: Int -- backoff before retrying a failed BF faucet operation
        _ -> 1
  threadDelay $ fromIntegral delay

seedFromFaucet ::
  ChainBackendOptions ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer IO FaucetLog ->
  IO UTxO
seedFromFaucet opts receivingVerificationKey val tracer = do
  seedFromFaucetWithMinting opts receivingVerificationKey val tracer Nothing

-- | Create a specially marked "seed" UTXO containing requested 'Value' by
-- redeeming funds available to the well-known faucet.
seedFromFaucetWithMinting ::
  ChainBackendOptions ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer IO FaucetLog ->
  Maybe PlutusScript ->
  IO UTxO
seedFromFaucetWithMinting opts receivingVerificationKey val tracer mintingScript =
  seedManyFromFaucetWithMinting opts [(receivingVerificationKey, val)] tracer mintingScript >>= \case
    [utxo] -> pure utxo
    utxos -> failure $ "seedFromFaucetWithMinting: expected one seeded UTxO, got " <> show (length utxos)

-- | Like 'seedFromFaucet' but pays every recipient from a single faucet
-- transaction. A scenario needing more than one funded key then waits for one
-- confirmation instead of one per key, which is a block time each on a public
-- network.
seedManyFromFaucet ::
  ChainBackendOptions ->
  -- | Recipients of the funds and the value each is to receive.
  [(VerificationKey PaymentKey, Value)] ->
  Tracer IO FaucetLog ->
  -- | The seeded UTxO of each recipient, in the order given.
  IO [UTxO]
seedManyFromFaucet opts recipients tracer =
  seedManyFromFaucetWithMinting opts recipients tracer Nothing

seedManyFromFaucetWithMinting ::
  ChainBackendOptions ->
  -- | Recipients of the funds and the value each is to receive.
  [(VerificationKey PaymentKey, Value)] ->
  Tracer IO FaucetLog ->
  Maybe PlutusScript ->
  -- | The seeded UTxO of each recipient, in the order given.
  IO [UTxO]
seedManyFromFaucetWithMinting _ [] _ _ = pure []
seedManyFromFaucetWithMinting opts recipients tracer mintingScript = do
  (faucetVk, faucetSk) <- keysFor Faucet
  networkId <- runBackend opts queryNetworkId
  seedTx <- retryOnExceptions tracer opts $ submitSeedTx faucetVk faucetSk networkId
  -- NOTE: The direct backend's 'awaitTransaction' yields all of the tx's
  -- outputs while the Blockfrost one yields only those paying to the given key,
  -- so ask per recipient and filter. Only the first call waits for the
  -- transaction, the rest are already satisfied by it.
  forM recipients $ \(vk, val) -> do
    seeded <-
      UTxO.filter (== toCtxUTxOTxOut (theOutput networkId vk val))
        <$> runBackend opts (awaitTransaction seedTx vk)
    -- An empty result means the output we asked for is not the one that landed
    -- (a value normalised on the way out, say). Say so here rather than let the
    -- caller trip over an empty UTxO much later.
    when (UTxO.null seeded) $
      failure $
        "seedManyFromFaucet: no output of " <> show val <> " for " <> show vk <> " in " <> show (getTxId $ getTxBody seedTx)
    pure seeded
 where
  submitSeedTx faucetVk faucetSk networkId = do
    faucetUTxO <- findFaucetUTxO networkId opts (sum $ selectLovelace . snd <$> recipients)
    let changeAddress = mkVkAddress networkId faucetVk
    let outputs = uncurry (theOutput networkId) <$> recipients

    runBackend opts (buildTransactionWithMintingScript changeAddress faucetUTxO (toList $ UTxO.inputSet faucetUTxO) outputs mintingScript) >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> do
        let signedTx = sign faucetSk (getTxBody tx)
        runBackend opts $ submitTransaction signedTx
        pure signedTx

  theOutput :: NetworkId -> VerificationKey PaymentKey -> Value -> TxOut CtxTx
  theOutput networkId vk val =
    TxOut
      (shelleyAddressInEra shelleyBasedEra (buildAddress vk networkId))
      val
      TxOutDatumNone
      ReferenceScriptNone

findFaucetUTxO :: NetworkId -> ChainBackendOptions -> Coin -> IO UTxO
findFaucetUTxO networkId opts lovelace = do
  (faucetVk, _) <- keysFor Faucet
  let address = buildAddress faucetVk networkId
  faucetUTxO <- runBackend opts $ queryUTxO [address]
  findUTxO faucetUTxO lovelace

seedFromFaucetBlockfrost ::
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Blockfrost.BlockfrostClientT IO UTxO
seedFromFaucetBlockfrost receivingVerificationKey lovelace = do
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
  faucetUTxO <- Blockfrost.queryUTxO networkId [changeAddress]
  foundUTxO <- findUTxO faucetUTxO lovelace
  case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId faucetVk) foundUTxO [] [theOutput] Nothing of
    Left e -> liftIO $ throwIO $ FaucetFailedToBuildTx{reason = e}
    Right tx -> do
      let signedTx = signTx faucetSk tx
      eResult <- Blockfrost.tryError $ Blockfrost.submitTransaction signedTx
      case eResult of
        Left err -> liftIO $ throwIO $ FaucetBlockfrostError{blockFrostError = show err}
        Right _ -> do
          void $ Blockfrost.awaitUTxO networkId [changeAddress] signedTx
          Blockfrost.awaitUTxO networkId [receivingAddress] signedTx

-- | Select enough entries to cover the given amount, largest first so the fewest
-- inputs are spent. NOTE: Selecting across entries rather than requiring a
-- single large enough one matters once a caller asks for more than one payout
-- at a time: the faucet's own change and the funds actors return to it are
-- individually smaller than such a sum, and would otherwise stop being usable.
findUTxO :: MonadIO m => UTxO.UTxO Era -> Lovelace -> m (UTxO.UTxO Era)
findUTxO utxo lovelace' =
  case select mempty 0 (sortOn (Down . lovelaceOf . snd) (UTxO.toList utxo)) of
    Nothing -> liftIO $ throwIO FaucetHasNotEnoughFunds{faucetUTxO = utxo}
    Just selected -> pure $ UTxO.fromList selected
 where
  select acc total = \case
    rest
      | total >= lovelace' -> Just acc
      | otherwise -> case rest of
          [] -> Nothing
          (o : os) -> select (o : acc) (total + lovelaceOf (snd o)) os

  lovelaceOf :: TxOut CtxUTxO -> Lovelace
  lovelaceOf = selectLovelace . txOutValue

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  ChainBackendOptions ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Tracer IO FaucetLog ->
  IO ()
seedFromFaucet_ opts vk ll tracer =
  void $ seedFromFaucet opts vk (lovelaceToValue ll) tracer

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  Tracer IO FaucetLog ->
  ChainBackendOptions ->
  Actor ->
  IO ()
returnFundsToFaucet tracer opts sender = do
  senderKeys <- keysFor sender
  void $ returnFundsToFaucet' tracer opts (snd senderKeys)

returnFundsToFaucet' ::
  Tracer IO FaucetLog ->
  ChainBackendOptions ->
  Secret (SigningKey PaymentKey) ->
  IO Coin
returnFundsToFaucet' tracer opts senderSk = do
  (faucetVk, _) <- keysFor Faucet
  networkId <- runBackend opts queryNetworkId
  let faucetAddress = mkVkAddress networkId faucetVk
  let senderVk = getVerificationKey senderSk
  utxo <- runBackend opts $ queryUTxOFor QueryTip senderVk
  returnAmount <-
    if UTxO.null utxo
      then pure 0
      else retryOnExceptions tracer opts $ do
        let utxoValue = balance @Tx utxo
        let allLovelace = selectLovelace utxoValue
        tx <- sign senderSk <$> buildTxBody utxo faucetAddress
        runBackend opts $ submitTransaction tx
        void $ runBackend opts $ awaitTransaction tx faucetVk
        pure allLovelace
  traceWith tracer $ ReturnedFunds{returnAmount}
  pure returnAmount
 where
  buildTxBody utxo faucetAddress =
    -- Here we specify no outputs in the transaction so that a change output with the
    -- entire value is created and paid to the faucet address.
    runBackend opts (buildTransaction faucetAddress utxo [] []) >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> pure $ getTxBody tx

-- Use the Faucet utxo to create the output at specified address
createOutputAtAddress ::
  NetworkId ->
  ChainBackendOptions ->
  AddressInEra ->
  TxOutDatum CtxTx ->
  Value ->
  IO (TxIn, TxOut CtxUTxO)
createOutputAtAddress networkId opts atAddress datum val = do
  (faucetVk, faucetSk) <- keysFor Faucet
  utxo <- findFaucetUTxO networkId opts (selectLovelace val)
  let collateralTxIns = mempty
  let output = TxOut atAddress val datum ReferenceScriptNone
  runBackend opts (buildTransaction (mkVkAddress networkId faucetVk) utxo collateralTxIns [output]) >>= \case
    Left e ->
      throwIO (ErrorAsException e)
    Right x -> do
      let body = getTxBody x
      let tx = withSecret faucetSk $ \rawSk -> makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey rawSk)] body
      runBackend opts $ submitTransaction tx
      newUtxo <- runBackend opts $ awaitTransaction tx faucetVk
      case UTxO.find (\out -> txOutAddress out == atAddress) newUtxo of
        Nothing -> failure $ "Could not find script output: " <> decodeUtf8 (encodePretty newUtxo)
        Just u -> pure u

-- | Try to submit tx and retry when some caught exception/s take place.
retryOnExceptions :: (MonadCatch m, MonadDelay m) => Tracer m FaucetLog -> ChainBackendOptions -> m a -> m a
retryOnExceptions tracer opts action =
  action
    `catches` [ Handler $ \(ex :: SubmitTransactionException) -> do
                  traceWith tracer $
                    SubmitTxError $
                      show ex
                  delayBF opts
                  retryOnExceptions tracer opts action
              , Handler $ \(ex :: IOException) -> do
                  unless (isResourceExhausted ex) $
                    throwIO ex
                  traceWith tracer $
                    TraceResourceExhaustedHandled $
                      "Expected exception raised from seedFromFaucet: " <> show ex
                  delayBF opts
                  retryOnExceptions tracer opts action
              ]
 where
  isResourceExhausted ex = case ioe_type ex of
    ResourceExhausted -> True
    _other -> False

-- | Publish current Hydra scripts as scripts outputs for later referencing them.
--
-- The key of the given Actor is used to pay for fees in required transactions,
-- it is expected to have sufficient funds.
publishHydraScriptsAs :: ChainBackendOptions -> Actor -> IO [TxId]
publishHydraScriptsAs opts actor = do
  (_, sk) <- keysFor actor
  runBackend opts $ publishHydraScripts (withSecret sk (mkSecret . CardanoSigningKey))

-- | Like 'publishHydraScriptsAs', but caches the resulting 'TxId's to a file
-- in the given directory. On subsequent calls, the cached 'TxId's are validated
-- against the chain (using 'queryScriptRegistry') and reused if still valid.
-- This avoids re-publishing identical scripts on every test run, saving funds
-- and time especially on public testnets.
publishOrReuseHydraScripts :: ChainBackendOptions -> Actor -> FilePath -> IO [TxId]
publishOrReuseHydraScripts opts actor cacheDir = do
  let cacheFile = cacheDir </> ".hydra-scripts-tx-ids"
  readCachedTxIds cacheFile >>= \case
    Just txIds -> do
      result <- try $ runBackend opts $ queryScriptRegistry txIds
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
    txIds <- publishHydraScriptsAs opts actor
    Aeson.encodeFile path txIds
    pure txIds
