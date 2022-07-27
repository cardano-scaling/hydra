{-# LANGUAGE DerivingStrategies #-}

module Hydra.Cluster.Faucet where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (
  CardanoClientException,
  QueryPoint (QueryTip),
  build,
  buildAddress,
  queryUTxO,
  sign,
  submit,
  waitForPayment,
 )
import CardanoNode (RunningNode (..))
import qualified Data.Map as Map
import Hydra.Chain.Direct.Util (isMarkedOutput, markerDatumHash, retry)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)

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
  IO UTxO
seedFromFaucet RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace marked = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retry isCardanoClientException $ submitSeedTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitSeedTx faucetVk faucetSk = do
    (i, _o) <- findUTxO faucetVk
    let changeAddress = buildAddress faucetVk networkId
    build networkId nodeSocket changeAddress [(i, Nothing)] [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        submit networkId nodeSocket (sign faucetSk body)

  findUTxO faucetVk = do
    faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
    let foundUTxO = find (\(_i, o) -> txOutLovelace o >= lovelace) $ UTxO.pairs faucetUTxO
    case foundUTxO of
      Just o -> pure o
      Nothing -> throwIO $ FaucetHasNotEnoughFunds{faucetUTxO}

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToValue lovelace)
      theOutputDatum

  theOutputDatum = case marked of
    Fuel -> TxOutDatumHash markerDatumHash
    Normal -> TxOutDatumNone

  isCardanoClientException :: CardanoClientException -> Bool
  isCardanoClientException = const True

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  IO ()
seedFromFaucet_ node vk ll marked =
  void $ seedFromFaucet node vk ll marked

-- | Query UTxO for the address of given verification key at point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFor :: RunningNode -> QueryPoint -> VerificationKey PaymentKey -> IO UTxO
queryUTxOFor RunningNode{networkId, nodeSocket} queryPoint vk =
  queryUTxO networkId nodeSocket queryPoint [buildAddress vk networkId]

-- | Like 'queryUTxOFor' at the tip, but also partition outputs marked as 'Fuel' and 'Normal'.
--
-- Throws at least 'QueryException' if query fails.
queryMarkedUTxO :: RunningNode -> VerificationKey PaymentKey -> IO (UTxO, UTxO)
queryMarkedUTxO node vk =
  mkPartition <$> queryUTxOFor node QueryTip vk
 where
  mkPartition = bimap UTxO UTxO . Map.partition isMarkedOutput . UTxO.toMap
