{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  RunningNode (..),
  SubmitTransactionException,
  awaitTransaction,
  awaitTransactionId,
  buildAddress,
  buildTransaction,
  buildTransactionWithPParams',
  queryUTxO,
  queryUTxOFor,
  sign,
  submitTransaction,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow (Handler (Handler), catches)
import Control.Tracer (Tracer, traceWith)
import Data.Set qualified as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Chain.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()
import Hydra.Options (DirectOptions (..))
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

-- | Create a specially marked "seed" UTXO containing requested 'Lovelace' by
-- redeeming funds available to the well-known faucet.
seedFromFaucet ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Tracer IO FaucetLog ->
  IO UTxO
seedFromFaucet node@RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace tracer = do
  (faucetVk, faucetSk) <- keysFor Faucet
  seedTx <- retryOnExceptions tracer $ submitSeedTx faucetVk faucetSk
  producedUTxO <- awaitTransaction networkId nodeSocket seedTx
  pure $ UTxO.filter (== toCtxUTxOTxOut theOutput) producedUTxO
 where
  submitSeedTx faucetVk faucetSk = do
    faucetUTxO <- findFaucetUTxO node lovelace
    let changeAddress = mkVkAddress networkId faucetVk
    buildTransaction networkId nodeSocket changeAddress faucetUTxO [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> do
        let signedTx = sign faucetSk $ getTxBody tx
        submitTransaction networkId nodeSocket signedTx
        pure signedTx

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra shelleyBasedEra receivingAddress)
      (lovelaceToValue lovelace)
      TxOutDatumNone
      ReferenceScriptNone

findFaucetUTxO :: RunningNode -> Coin -> IO UTxO
findFaucetUTxO RunningNode{networkId, nodeSocket} lovelace = do
  (faucetVk, _) <- keysFor Faucet
  faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
  let foundUTxO = UTxO.filter (\o -> (selectLovelace . txOutValue) o >= lovelace) faucetUTxO
  when (null foundUTxO) $
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
  case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId faucetVk) foundUTxO [] [theOutput] of
    Left e -> liftIO $ throwIO $ FaucetFailedToBuildTx{reason = e}
    Right tx -> do
      let signedTx = signTx faucetSk tx
      eResult <- Blockfrost.tryError $ Blockfrost.submitTransaction signedTx
      case eResult of
        Left err -> liftIO $ throwIO $ FaucetBlockfrostError{blockFrostError = show err}
        Right _ -> do
          void $ Blockfrost.awaitUTxO networkId [changeAddress] (txId signedTx) 100
          Blockfrost.awaitUTxO networkId [receivingAddress] (txId signedTx) 100
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
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Coin ->
  Tracer IO FaucetLog ->
  IO ()
seedFromFaucet_ node vk ll tracer =
  void $ seedFromFaucet node vk ll tracer

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  Tracer IO FaucetLog ->
  RunningNode ->
  Actor ->
  IO ()
returnFundsToFaucet tracer node sender = do
  senderKeys <- keysFor sender
  void $ returnFundsToFaucet' tracer node (snd senderKeys)

returnFundsToFaucet' ::
  Tracer IO FaucetLog ->
  RunningNode ->
  SigningKey PaymentKey ->
  IO Coin
returnFundsToFaucet' tracer RunningNode{networkId, nodeSocket} senderSk = do
  (faucetVk, _) <- keysFor Faucet
  let faucetAddress = mkVkAddress networkId faucetVk
  let senderVk = getVerificationKey senderSk
  utxo <- queryUTxOFor networkId nodeSocket QueryTip senderVk
  returnAmount <-
    if null utxo
      then pure 0
      else retryOnExceptions tracer $ do
        let utxoValue = balance @Tx utxo
        let allLovelace = selectLovelace utxoValue
        tx <- sign senderSk <$> buildTxBody utxo faucetAddress
        submitTransaction networkId nodeSocket tx
        void $ awaitTransaction networkId nodeSocket tx
        pure allLovelace
  traceWith tracer $ ReturnedFunds{returnAmount}
  pure returnAmount
 where
  buildTxBody utxo faucetAddress =
    -- Here we specify no outputs in the transaction so that a change output with the
    -- entire value is created and paid to the faucet address.
    buildTransaction networkId nodeSocket faucetAddress utxo [] [] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> pure $ getTxBody tx

-- Use the Faucet utxo to create the output at specified address
createOutputAtAddress ::
  RunningNode ->
  AddressInEra ->
  TxOutDatum CtxTx ->
  Value ->
  IO (TxIn, TxOut CtxUTxO)
createOutputAtAddress node@RunningNode{networkId, nodeSocket} atAddress datum val = do
  (faucetVk, faucetSk) <- keysFor Faucet
  utxo <- findFaucetUTxO node 0
  let collateralTxIns = mempty
  let output = TxOut atAddress val datum ReferenceScriptNone
  buildTransaction
    networkId
    nodeSocket
    (changeAddress faucetVk)
    utxo
    collateralTxIns
    [output]
    >>= \case
      Left e ->
        throwErrorAsException e
      Right x -> do
        let body = getTxBody x
        let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey faucetSk)] body
        submitTransaction networkId nodeSocket tx
        newUtxo <- awaitTransaction networkId nodeSocket tx
        case UTxO.find (\out -> txOutAddress out == atAddress) newUtxo of
          Nothing -> failure $ "Could not find script output: " <> decodeUtf8 (encodePretty newUtxo)
          Just u -> pure u
 where
  changeAddress = mkVkAddress networkId

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
publishHydraScriptsAs :: RunningNode -> Actor -> IO [TxId]
publishHydraScriptsAs RunningNode{networkId, nodeSocket} actor = do
  (_, sk) <- keysFor actor
  txIds <- publishHydraScripts (DirectBackend $ DirectOptions{networkId, nodeSocket}) sk
  mapM_ (awaitTransactionId networkId nodeSocket) txIds
  pure txIds
