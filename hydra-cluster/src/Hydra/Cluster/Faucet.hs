module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Core (PParams)
import CardanoClient (
  QueryPoint (QueryTip),
  SubmitTransactionException,
  awaitTransaction,
  buildAddress,
  buildTransaction,
  queryUTxO,
  queryUTxOFor,
  sign,
  submitTransaction,
  waitForPayment,
 )
import CardanoNode (RunningNode (..))
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow (Handler (Handler), catches)
import Control.Tracer (Tracer, traceWith)
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Direct.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Cluster.Fixture (Actor (Faucet), actorName)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano ()

data FaucetException
  = FaucetHasNotEnoughFunds {faucetUTxO :: UTxO}
  | FaucetFailedToBuildTx {reason :: TxBodyErrorAutoBalance}
  deriving stock (Show)

instance Exception FaucetException

data FaucetLog
  = TraceResourceExhaustedHandled Text
  | ReturnedFunds {actor :: String, returnAmount :: Lovelace}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a specially marked "seed" UTXO containing requested 'Lovelace' by
-- redeeming funds available to the well-known faucet.
seedFromFaucet ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  Tracer IO FaucetLog ->
  IO UTxO
seedFromFaucet node@RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace tracer = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retryOnExceptions tracer $ submitSeedTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitSeedTx faucetVk faucetSk = do
    faucetUTxO <- findFaucetUTxO node lovelace
    let changeAddress = ShelleyAddressInEra (buildAddress faucetVk networkId)
    buildTransaction networkId nodeSocket changeAddress faucetUTxO [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        submitTransaction networkId nodeSocket (sign faucetSk body)

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra shelleyBasedEra receivingAddress)
      (lovelaceToValue lovelace)
      TxOutDatumNone
      ReferenceScriptNone

findFaucetUTxO :: RunningNode -> Lovelace -> IO UTxO
findFaucetUTxO RunningNode{networkId, nodeSocket} lovelace = do
  (faucetVk, _) <- keysFor Faucet
  faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
  let foundUTxO = UTxO.filter (\o -> txOutLovelace o >= lovelace) faucetUTxO
  when (null foundUTxO) $
    throwIO $
      FaucetHasNotEnoughFunds{faucetUTxO}
  pure foundUTxO

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  Tracer IO FaucetLog ->
  IO ()
seedFromFaucet_ node vk ll tracer =
  void $ seedFromFaucet node vk ll tracer

-- TODO(Elaine): we probably want a simplified but parallel version  of this/createOutputAddress for offline mode , that just constructs a UTxO
-- actually no take a look at seedFromFaucet

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  Tracer IO FaucetLog ->
  RunningNode ->
  Actor ->
  IO ()
returnFundsToFaucet tracer node@RunningNode{networkId, nodeSocket} sender = do
  (faucetVk, _) <- keysFor Faucet
  let faucetAddress = mkVkAddress networkId faucetVk

  (senderVk, senderSk) <- keysFor sender
  utxo <- queryUTxOFor networkId nodeSocket QueryTip senderVk

  retryOnExceptions tracer $ do
    let utxoValue = balance @Tx utxo
    let allLovelace = selectLovelace utxoValue
    -- select tokens other than ADA here so we can burn it afterwards
    let otherTokens = filterValue (/= AdaAssetId) utxoValue
    -- XXX: Using a hard-coded high-enough value to satisfy the min utxo value.
    -- NOTE: We use the faucet address as the change deliberately here.
    fee <- calculateTxFee node senderSk utxo faucetAddress 1_000_000
    let returnBalance = allLovelace - fee
    tx <- sign senderSk <$> buildTxBody utxo faucetAddress returnBalance otherTokens
    submitTransaction networkId nodeSocket tx
    void $ awaitTransaction networkId nodeSocket tx
    traceWith tracer $ ReturnedFunds{actor = actorName sender, returnAmount = returnBalance}
 where
  buildTxBody utxo faucetAddress lovelace otherTokens =
    let theOutput = TxOut faucetAddress (lovelaceToValue lovelace <> negateValue otherTokens) TxOutDatumNone ReferenceScriptNone
     in buildTransaction networkId nodeSocket faucetAddress utxo [] [theOutput] >>= \case
          Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
          Right body -> pure body

-- Use the Faucet utxo to create the output at specified address
createOutputAtAddress ::
  RunningNode ->
  PParams LedgerEra ->
  AddressInEra ->
  TxOutDatum CtxTx ->
  IO (TxIn, TxOut CtxUTxO)
createOutputAtAddress node@RunningNode{networkId, nodeSocket} pparams atAddress datum = do
  (faucetVk, faucetSk) <- keysFor Faucet
  -- we don't care which faucet utxo we use here so just pass lovelace 0 to grab
  -- any present utxo
  utxo <- findFaucetUTxO node 0
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
      Right body -> do
        let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey faucetSk)] body
        submitTransaction networkId nodeSocket tx
        newUtxo <- awaitTransaction networkId nodeSocket tx
        case UTxO.find (\out -> txOutAddress out == atAddress) newUtxo of
          Nothing -> failure $ "Could not find script output: " <> decodeUtf8 (encodePretty newUtxo)
          Just u -> pure u
 where
  collateralTxIns = mempty

  output =
    mkTxOutAutoBalance
      pparams
      atAddress
      mempty
      datum
      ReferenceScriptNone

  changeAddress = mkVkAddress networkId

-- | Build and sign tx and return the calculated fee.
-- - Signing key should be the key of a sender
-- - Address is used as a change address.
-- - Lovelace amount should be one we are trying to send.
calculateTxFee ::
  RunningNode ->
  SigningKey PaymentKey ->
  UTxO ->
  AddressInEra ->
  Lovelace ->
  IO Lovelace
calculateTxFee RunningNode{networkId, nodeSocket} secretKey utxo addr lovelace =
  let theOutput = TxOut addr (lovelaceToValue lovelace) TxOutDatumNone ReferenceScriptNone
   in buildTransaction networkId nodeSocket addr utxo [] [theOutput] >>= \case
        Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
        Right body -> pure $ txFee' (sign secretKey body)

-- | Try to submit tx and retry when some caught exception/s take place.
retryOnExceptions :: (MonadCatch m, MonadDelay m) => Tracer m FaucetLog -> m () -> m ()
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
publishHydraScriptsAs :: RunningNode -> Actor -> IO TxId
publishHydraScriptsAs RunningNode{networkId, nodeSocket} actor = do
  (_, sk) <- keysFor actor
  publishHydraScripts networkId nodeSocket sk
