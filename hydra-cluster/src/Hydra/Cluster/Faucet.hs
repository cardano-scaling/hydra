{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  queryUTxO,
  sign,
  waitForPayment,
 )
import CardanoNode (RunningNode (..))
import Control.Exception (IOException)
import Control.Monad.Class.MonadThrow (Handler (Handler), catches)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Map as Map
import GHC.IO.Exception (IOErrorType (ResourceExhausted), IOException (ioe_type))
import Hydra.Chain.CardanoClient (
  SubmitTransactionException,
  buildTransaction,
  queryUTxOFor,
  submitTransaction,
 )
import Hydra.Chain.Direct.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Chain.Direct.Util (isMarkedOutput, markerDatumHash)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()

data Marked = Fuel | Normal

data FaucetException
  = NotEnoughFunds {utxos :: UTxO, requestedAmount :: Lovelace}
  | FailedToBuildTx {reason :: TxBodyErrorAutoBalance}
  deriving (Show)

instance Exception FaucetException

newtype FaucetLog
  = TraceResourceExhaustedHandled Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

sendFundsTo ::
  RunningNode ->
  -- | Sender keys
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | Receiving verification key
  VerificationKey PaymentKey ->
  -- | Amount to return to faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  Tracer IO FaucetLog ->
  -- | Should we filter utxo to find the one containing more lovelace than we want to send?
  Bool ->
  IO UTxO
sendFundsTo cardanoNode@RunningNode{networkId, nodeSocket} (senderVk, senderSk) receivingVerificationKey lovelace marked tracer shouldFilter = do
  retryOnExceptions tracer $
    buildAndSubmitTx cardanoNode lovelace marked receivingAddress senderVk senderSk shouldFilter
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  receivingAddress = buildAddress receivingVerificationKey networkId

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
seedFromFaucet cardanoNode@RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace marked tracer = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retryOnExceptions tracer $
    buildAndSubmitTx cardanoNode lovelace marked receivingAddress faucetVk faucetSk True
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  receivingAddress = buildAddress receivingVerificationKey networkId

buildAndSubmitTx ::
  RunningNode ->
  -- | Amount of lovelace to send to the reciver
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  -- | Receiving address
  Address ShelleyAddr ->
  -- | Sender verification key
  VerificationKey PaymentKey ->
  -- | Sender signing key
  SigningKey PaymentKey ->
  -- | Should we filter utxo to find the one containing more lovelace than we want to send?
  Bool ->
  IO ()
buildAndSubmitTx cardanoNode@RunningNode{networkId, nodeSocket} lovelace marked receivingAddress senderVk senderSk shouldFilter = do
  utxo <- findUTxO cardanoNode lovelace senderVk shouldFilter
  let changeAddress = ShelleyAddressInEra (buildAddress senderVk networkId)
  buildTransaction networkId nodeSocket changeAddress utxo [] [theOutput] >>= \case
    Left e -> throwIO $ FailedToBuildTx{reason = e}
    Right body -> do
      submitTransaction networkId nodeSocket (sign senderSk body)
 where
  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToValue lovelace)
      theOutputDatum
      ReferenceScriptNone

  theOutputDatum = case marked of
    Fuel -> TxOutDatumHash markerDatumHash
    Normal -> TxOutDatumNone

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

-- | Find the utxo for the corresponding verification key
-- We expect proper utxo to have more 'Lovelace' than the @lovelace@ argument
findUTxO :: RunningNode -> Lovelace -> VerificationKey PaymentKey -> Bool -> IO UTxO
findUTxO RunningNode{networkId, nodeSocket} lovelace faucetVk shouldFilter = do
  utxos <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
  let foundUTxO =
        if shouldFilter
          then UTxO.filter (\o -> txOutLovelace o >= lovelace) utxos
          else utxos
  when (null foundUTxO) $
    throwIO $ NotEnoughFunds{utxos, requestedAmount = lovelace}
  pure foundUTxO

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
