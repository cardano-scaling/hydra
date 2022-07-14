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
  waitForTransaction,
 )
import CardanoNode (RunningNode (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Chain.Direct.Util (isMarkedOutput, markerDatumHash, retry)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import qualified Hydra.Contract.Initial as Initial
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
  IO UTxO
seedFromFaucet RunningNode{networkId, nodeSocket} receivingVerificationKey lovelace marked = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retry isCardanoClientException $ submitSeedTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitSeedTx faucetVk faucetSk = do
    faucetUTxO <- findUTxO faucetVk
    let changeAddress = buildAddress faucetVk networkId
    build networkId nodeSocket changeAddress faucetUTxO [] [theOutput] >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        submit networkId nodeSocket (sign faucetSk body)

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

-- | Publish Hydra scripts as scripts outputs for later referencing them.
--
-- The given key is used to pay for fees in required transactions, it is
-- expected to have funds.
publishHydraScripts :: NetworkId -> RunningNode -> SigningKey PaymentKey -> IO TxId
publishHydraScripts networkId node@(RunningNode _ nodeSocket) sk = do
  utxo <- queryUTxOFor networkId node QueryTip vk
  let someTxIn = Set.findMin $ UTxO.inputSet utxo
  build
    networkId
    nodeSocket
    changeAddress
    [(someTxIn, Nothing)]
    []
    [publishInitial]
    >>= \case
      Left e ->
        throwErrorAsException e
      Right body -> do
        let tx = sign sk body
        submit networkId nodeSocket tx
        utxo <- waitForTransaction networkId nodeSocket tx
        print (encodePretty utxo)
        return $ getTxId body
 where
  changeAddress = buildAddress vk networkId
  vk = getVerificationKey sk

  publishInitial :: TxOut CtxTx
  publishInitial =
    TxOut
      (shelleyAddressInEra changeAddress) -- FIXME: Can be whatever we want, but ideally a 'sink' address that can't be spent
      probablyEnoughAda
      TxOutDatumNone
      (ReferenceScript (toScriptInAnyLang $ PlutusScript (fromPlutusScript Initial.validatorScript)))

  -- This depends on protocol parameters and the size of the script.
  -- TODO: Calculate this value instead from pparams.
  probablyEnoughAda =
    lovelaceToValue 23_437_780

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
