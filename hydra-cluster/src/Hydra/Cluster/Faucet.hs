{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
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
import qualified Data.Map as Map
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.Direct.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Chain.Direct.Util (isMarkedOutput, markerDatumHash)
import Hydra.Cluster.Fixture (Actor (Faucet), actorName)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano ()

data Marked = Fuel | Normal

data FaucetException
  = FaucetHasNotEnoughFunds {faucetUTxO :: UTxO}
  | FaucetFailedToBuildTx {reason :: TxBodyErrorAutoBalance}
  deriving (Show)

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
  -- | Marked as fuel or normal output?
  Marked ->
  Tracer IO FaucetLog ->
  IO UTxO
seedFromFaucet RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace marked tracer = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retryOnExceptions tracer $ submitSeedTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitSeedTx faucetVk faucetSk = do
    faucetUTxO <- findUTxO faucetVk
    let changeAddress = ShelleyAddressInEra (buildAddress faucetVk networkId)
    buildTransaction networkId nodeSocket changeAddress faucetUTxO [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        submitTransaction networkId nodeSocket (sign faucetSk body)

  findUTxO faucetVk = do
    faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
    let foundUTxO = UTxO.filter (\o -> txOutLovelace o >= lovelace) faucetUTxO
    when (null foundUTxO) $
      throwIO $
        FaucetHasNotEnoughFunds{faucetUTxO}
    pure foundUTxO

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToValue lovelace)
      theOutputDatum
      ReferenceScriptNone

  theOutputDatum = case marked of
    Fuel -> TxOutDatumHash markerDatumHash
    Normal -> TxOutDatumNone

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  Tracer IO FaucetLog ->
  IO ()
seedFromFaucet_ node vk ll marked tracer =
  void $ seedFromFaucet node vk ll marked tracer

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
    let allLovelace = selectLovelace $ balance @Tx utxo
    -- XXX: Using a hard-coded high-enough value to satisfy the min utxo value.
    -- NOTE: We use the faucet address as the change deliberately here.
    fee <- calculateTxFee node senderSk utxo faucetAddress 1_000_000
    let returnBalance = allLovelace - fee
    tx <- sign senderSk <$> buildTxBody utxo faucetAddress returnBalance
    submitTransaction networkId nodeSocket tx
    void $ awaitTransaction networkId nodeSocket tx
    traceWith tracer $ ReturnedFunds{actor = actorName sender, returnAmount = returnBalance}
 where
  buildTxBody utxo faucetAddress lovelace =
    let theOutput = TxOut faucetAddress (lovelaceToValue lovelace) TxOutDatumNone ReferenceScriptNone
     in buildTransaction networkId nodeSocket faucetAddress utxo [] [theOutput] >>= \case
          Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
          Right body -> pure body

-- | Build and sign tx and return the calculated fee.
-- - Signing key should be the key of a sender
-- - Address is used as a change address.
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

-- | Like 'queryUTxOFor' at the tip, but also partition outputs marked as 'Fuel' and 'Normal'.
--
-- Throws at least 'QueryException' if query fails.
queryMarkedUTxO :: RunningNode -> VerificationKey PaymentKey -> IO (UTxO, UTxO)
queryMarkedUTxO RunningNode{nodeSocket, networkId} vk =
  mkPartition <$> queryUTxOFor networkId nodeSocket QueryTip vk
 where
  mkPartition = bimap UTxO UTxO . Map.partition isMarkedOutput . UTxO.toMap
