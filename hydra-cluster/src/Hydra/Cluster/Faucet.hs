{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

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
import CardanoNode (NodeLog (MsgCLIStatus), RunningNode (..))
import Control.Exception (IOException)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Map as Map
import Hydra.Chain.CardanoClient (
  SubmitTransactionException,
  buildTransaction,
  queryUTxOFor,
  submitTransaction,
 )
import Hydra.Chain.Direct.ScriptRegistry (
  publishHydraScripts,
 )
import Hydra.Chain.Direct.Util (isMarkedOutput, markerDatumHash, retry, retryWithPreAction)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano ()

data Marked = Fuel | Normal

data FaucetException
  = FaucetHasNotEnoughFunds {faucetUTxO :: UTxO}
  | FaucetFailedToBuildTx {reason :: TxBodyErrorAutoBalance}
  deriving (Show)

instance Exception FaucetException

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
  Tracer IO NodeLog ->
  IO UTxO
seedFromFaucet RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace marked tracer = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retryWithPreAction isIOException preAction $
    retry isSubmitTransactionException $ submitSeedTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  preAction :: Exception ex => ex -> IO ()
  preAction e = traceWith tracer $ MsgCLIStatus "[RETRY]" (show e)

  submitSeedTx faucetVk faucetSk = do
    traceWith tracer $ MsgCLIStatus "[HERE]" "finding faucetUTxO"
    faucetUTxO <- findUTxO faucetVk -- maybe this should be done once globally and not per each thread
    traceWith tracer $ MsgCLIStatus "[HERE]" "faucetUTxO found"
    let changeAddress = ShelleyAddressInEra (buildAddress faucetVk networkId)
    buildTransaction networkId nodeSocket changeAddress faucetUTxO [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        traceWith tracer $ MsgCLIStatus "[HERE]" "tx built"
        submitTransaction networkId nodeSocket (sign faucetSk body) `catch` (printExceptionAndThrow @SubmitTransactionException)
        traceWith tracer $ MsgCLIStatus "[HERE]" "tx submitted"

  printExceptionAndThrow :: Exception a => a -> IO ()
  printExceptionAndThrow = \ex -> do
    traceWith tracer $ MsgCLIStatus "[ERROR]" ("other:" <> show ex)
    throwIO ex

  findUTxO faucetVk = do
    faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
    let foundUTxO = UTxO.filter (\o -> txOutLovelace o >= lovelace) faucetUTxO
    when (null foundUTxO) $
      throwIO $ FaucetHasNotEnoughFunds{faucetUTxO}
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

  isSubmitTransactionException :: SubmitTransactionException -> Bool
  isSubmitTransactionException = const True

  isIOException :: IOException -> Bool
  isIOException = const True

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  Tracer IO NodeLog ->
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
