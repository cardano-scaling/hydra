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
import Hydra.Chain.Direct.Util (markerDatumHash, retry)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)

data Marked = Fuel | Normal

-- | Create a specially marked "seed" UTXO containing requested 'Lovelace' by
-- redeeming funds available to the well-known faucet.
--
-- NOTE: This function is querying and looping forever until it finds a suitable
-- output!
seedFromFaucet ::
  NetworkId ->
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  IO UTxO
seedFromFaucet networkId (RunningNode _ nodeSocket) receivingVerificationKey lovelace marked = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retry isCardanoClientException $ submitFuelingTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitFuelingTx faucetVk faucetSk = do
    (i, _o) <- findUTxO faucetVk
    let changeAddress = buildAddress faucetVk networkId
    build networkId nodeSocket changeAddress [(i, Nothing)] [] [theOutput] >>= \case
      Left e -> error (show e)
      Right body -> do
        submit networkId nodeSocket (sign faucetSk body)

  findUTxO faucetVk = do
    faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
    let foundUTxO = find (\(_i, o) -> txOutLovelace o >= lovelace) $ UTxO.pairs faucetUTxO
    case foundUTxO of
      Just o -> pure o
      Nothing ->
        findUTxO faucetVk

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
  NetworkId ->
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  IO ()
seedFromFaucet_ nid node vk ll marked =
  void $ seedFromFaucet nid node vk ll marked
