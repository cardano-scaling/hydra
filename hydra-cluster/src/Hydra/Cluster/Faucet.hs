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
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithMintingScript, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()
import Hydra.Tx (balance, txId)

data FaucetException
  = FaucetHasNotEnoughFunds {faucetUTxO :: UTxO}
  | FaucetFailedToBuildTx {reason :: TxBodyErrorAutoBalance Era}
  | FaucetBlockfrostError {blockFrostError :: Text}
  deriving stock (Show)

instance Exception FaucetException

data FaucetLog
  = TraceResourceExhaustedHandled Text
  | ReturnedFunds {returnAmount :: Coin}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

seedFromFaucet ::
  ChainBackend backend =>
  backend ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer IO FaucetLog ->
  IO UTxO
seedFromFaucet backend receivingVerificationKey val tracer = do
  seedFromFaucetWithMinting backend receivingVerificationKey val tracer Nothing

-- | Create a specially marked "seed" UTXO containing requested 'Value' by
-- redeeming funds available to the well-known faucet.
seedFromFaucetWithMinting ::
  ChainBackend backend =>
  backend ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Value to get from faucet
  Value ->
  Tracer IO FaucetLog ->
  Maybe PlutusScript ->
  IO UTxO
seedFromFaucetWithMinting backend receivingVerificationKey val tracer mintingScript = do
  (faucetVk, faucetSk) <- keysFor Faucet
  networkId <- Backend.queryNetworkId backend
  seedTx <- retryOnExceptions tracer $ submitSeedTx faucetVk faucetSk networkId
  producedUTxO <- Backend.awaitTransaction backend seedTx receivingVerificationKey
  pure $ UTxO.filter (== toCtxUTxOTxOut (theOutput networkId)) producedUTxO
 where
  submitSeedTx faucetVk faucetSk networkId = do
    faucetUTxO <- findFaucetUTxO networkId backend (selectLovelace val)
    let changeAddress = mkVkAddress networkId faucetVk

    buildTransactionWithMintingScript backend changeAddress faucetUTxO (toList $ UTxO.inputSet faucetUTxO) [theOutput networkId] mintingScript >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> do
        let signedTx = sign faucetSk $ getTxBody tx
        Backend.submitTransaction backend signedTx
        pure signedTx

  receivingAddress = buildAddress receivingVerificationKey

  theOutput networkId =
    TxOut
      (shelleyAddressInEra shelleyBasedEra (receivingAddress networkId))
      val
      TxOutDatumNone
      ReferenceScriptNone

findFaucetUTxO :: ChainBackend backend => NetworkId -> backend -> Coin -> IO UTxO
findFaucetUTxO networkId backend lovelace = do
  (faucetVk, _) <- keysFor Faucet
  faucetUTxO <- Backend.queryUTxO backend [buildAddress faucetVk networkId]
  let foundUTxO = UTxO.filter (\o -> (selectLovelace . txOutValue) o >= lovelace) faucetUTxO
  when (UTxO.null foundUTxO) $
    throwIO $
      FaucetHasNotEnoughFunds{faucetUTxO}
  pure foundUTxO

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
  foundUTxO <- findUTxO networkId changeAddress lovelace
  case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId faucetVk) foundUTxO [] [theOutput] Nothing of
    Left e -> liftIO $ throwIO $ FaucetFailedToBuildTx{reason = e}
    Right tx -> do
      let signedTx = signTx faucetSk tx
      eResult <- Blockfrost.tryError $ Blockfrost.submitTransaction signedTx
      case eResult of
        Left err -> liftIO $ throwIO $ FaucetBlockfrostError{blockFrostError = show err}
        Right _ -> do
          void $ Blockfrost.awaitUTxO networkId [changeAddress] (txId signedTx) 200
          Blockfrost.awaitUTxO networkId [receivingAddress] (txId signedTx) 200
 where
  findUTxO networkId address lovelace' = do
    faucetUTxO <- Blockfrost.queryUTxO networkId [address]
    let foundUTxO = UTxO.find (\o -> (selectLovelace . txOutValue) o >= lovelace') faucetUTxO
    when (isNothing foundUTxO) $
      liftIO $
        throwIO $
          FaucetHasNotEnoughFunds{faucetUTxO}
    pure $ maybe mempty (uncurry UTxO.singleton) foundUTxO

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  ChainBackend backend =>
  backend ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Tracer IO FaucetLog ->
  IO ()
seedFromFaucet_ backend vk ll tracer =
  void $ seedFromFaucet backend vk (lovelaceToValue ll) tracer

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  ChainBackend backend =>
  Tracer IO FaucetLog ->
  backend ->
  Actor ->
  IO ()
returnFundsToFaucet tracer backend sender = do
  senderKeys <- keysFor sender
  void $ returnFundsToFaucet' tracer backend (snd senderKeys)

returnFundsToFaucet' ::
  ChainBackend backend =>
  Tracer IO FaucetLog ->
  backend ->
  SigningKey PaymentKey ->
  IO Coin
returnFundsToFaucet' tracer backend senderSk = do
  (faucetVk, _) <- keysFor Faucet
  networkId <- Backend.queryNetworkId backend
  let faucetAddress = mkVkAddress networkId faucetVk
  let senderVk = getVerificationKey senderSk
  utxo <- Backend.queryUTxOFor backend QueryTip senderVk
  returnAmount <-
    if UTxO.null utxo
      then pure 0
      else retryOnExceptions tracer $ do
        let utxoValue = balance @Tx utxo
        let allLovelace = selectLovelace utxoValue
        tx <- sign senderSk <$> buildTxBody utxo faucetAddress
        Backend.submitTransaction backend tx
        void $ Backend.awaitTransaction backend tx faucetVk
        pure allLovelace
  traceWith tracer $ ReturnedFunds{returnAmount}
  pure returnAmount
 where
  buildTxBody utxo faucetAddress =
    -- Here we specify no outputs in the transaction so that a change output with the
    -- entire value is created and paid to the faucet address.
    buildTransaction backend faucetAddress utxo [] [] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> pure $ getTxBody tx

-- Use the Faucet utxo to create the output at specified address
createOutputAtAddress ::
  ChainBackend backend =>
  NetworkId ->
  backend ->
  AddressInEra ->
  TxOutDatum CtxTx ->
  Value ->
  IO (TxIn, TxOut CtxUTxO)
createOutputAtAddress networkId backend atAddress datum val = do
  (faucetVk, faucetSk) <- keysFor Faucet
  utxo <- findFaucetUTxO networkId backend (selectLovelace val)
  let collateralTxIns = mempty
  let output = TxOut atAddress val datum ReferenceScriptNone
  buildTransaction backend (mkVkAddress networkId faucetVk) utxo collateralTxIns [output] >>= \case
    Left e ->
      throwErrorAsException e
    Right x -> do
      let body = getTxBody x
      let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey faucetSk)] body
      Backend.submitTransaction backend tx
      newUtxo <- Backend.awaitTransaction backend tx faucetVk
      case UTxO.find (\out -> txOutAddress out == atAddress) newUtxo of
        Nothing -> failure $ "Could not find script output: " <> decodeUtf8 (encodePretty newUtxo)
        Just u -> pure u

-- | Try to submit tx and retry when some caught exception/s take place.
retryOnExceptions :: (MonadCatch m, MonadDelay m) => Tracer m FaucetLog -> m a -> m a
retryOnExceptions tracer action =
  action
    `catches` [ Handler $ \(_ :: SubmitTransactionException) -> do
                  threadDelay 1
                  retryOnExceptions tracer action
              , Handler $ \(ex :: IOException) -> do
                  unless (isResourceExhausted ex) $
                    throwIO ex
                  traceWith tracer $
                    TraceResourceExhaustedHandled $
                      "Expected exception raised from seedFromFaucet: " <> show ex
                  threadDelay 1
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
publishHydraScriptsAs :: ChainBackend backend => backend -> Actor -> IO [TxId]
publishHydraScriptsAs backend actor = do
  (_, sk) <- keysFor actor
  publishHydraScripts backend sk
