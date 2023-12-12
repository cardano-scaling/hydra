{-# LANGUAGE DuplicateRecordFields #-}

-- | A cardano-node client used in end-to-end tests and benchmarks.
--
-- This modules contains some more functions besides the re-exported basic
-- querying of hydra-node's 'Hydra.Chain.CardanoClient'.
module CardanoClient (
  module Hydra.Chain.CardanoClient,
  module CardanoClient,
) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block)
import Hydra.Chain.CardanoClient

import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Chain.CardanoClient qualified as CardanoClient

-- TODO(SN): DRY with Hydra.Cardano.Api

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

buildScriptAddress :: Script -> NetworkId -> Address ShelleyAddr
buildScriptAddress script networkId =
  let hashed = hashScript script
   in makeShelleyAddress networkId (PaymentCredentialByScript hashed) NoStakeAddress

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut CtxTx] -> Either TxBodyError TxBody
buildRaw ins outs =
  createAndValidateTransactionBody $
    defaultTxBodyContent
      & setTxIns (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) ins)
      & setTxOuts outs

data Sizes = Sizes
  { inputs :: Int
  , outputs :: Int
  , witnesses :: Int
  }
  deriving stock (Eq, Show)

defaultSizes :: Sizes
defaultSizes = Sizes{inputs = 0, outputs = 0, witnesses = 0}

-- | Sign a transaction body with given signing key.
sign :: SigningKey PaymentKey -> TxBody -> Tx
sign signingKey body =
  makeSignedTransaction
    [makeShelleyKeyWitness body (WitnessPaymentKey signingKey)]
    body

-- | Submit a transaction to a 'RunningNode'
submitTx :: RunningNode -> Tx -> IO ()
submitTx RunningNode{networkId, nodeSocket} =
  submitTransaction networkId nodeSocket

-- | Wait until the specified Address has received payments, visible on-chain,
-- for the specified Lovelace amount. Returns the UTxO set containing all payments
-- with the same Lovelace amount at the given Address.
--
-- Note that this function loops indefinitely; therefore, it's recommended to use
-- it with a surrounding timeout mechanism.
waitForPayments ::
  NetworkId ->
  SocketPath ->
  Coin ->
  Address ShelleyAddr ->
  IO UTxO
waitForPayments networkId socket amount addr =
  go
 where
  go = do
    utxo <- queryUTxO networkId socket QueryTip [addr]
    let expectedPayments = selectPayments utxo
    if expectedPayments /= mempty
      then pure $ UTxO expectedPayments
      else threadDelay 1 >> go

  selectPayments (UTxO utxo) =
    Map.filter ((== amount) . selectLovelace . txOutValue) utxo

-- | Wait for transaction outputs with matching lovelace value and addresses of
-- the whole given UTxO
waitForUTxO ::
  RunningNode ->
  UTxO ->
  IO ()
waitForUTxO node utxo =
  forM_ (snd <$> UTxO.pairs utxo) forEachUTxO
 where
  RunningNode{networkId, nodeSocket} = node

  forEachUTxO :: TxOut CtxUTxO -> IO ()
  forEachUTxO = \case
    TxOut (ShelleyAddressInEra addr@ShelleyAddress{}) value _ _ -> do
      void $
        waitForPayments
          networkId
          nodeSocket
          (selectLovelace value)
          addr
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

mkGenesisTx ::
  NetworkId ->
  -- | Owner of the 'initialFund'.
  SigningKey PaymentKey ->
  -- | Amount of initialFunds
  Coin ->
  -- | Recipients and amounts to pay in this transaction.
  [(VerificationKey PaymentKey, Coin)] ->
  Tx
mkGenesisTx networkId signingKey initialAmount recipients =
  case buildRaw [initialInput] (recipientOutputs <> [changeOutput]) of
    Left err -> error $ "Fail to build genesis transations: " <> show err
    Right tx -> sign signingKey tx
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ getVerificationKey signingKey)

  totalSent = foldMap snd recipients

  changeAddr = mkVkAddress networkId (getVerificationKey signingKey)
  changeOutput =
    TxOut
      changeAddr
      (lovelaceToValue $ initialAmount - totalSent)
      TxOutDatumNone
      ReferenceScriptNone

  recipientOutputs =
    flip map recipients $ \(vk, ll) ->
      TxOut
        (mkVkAddress networkId vk)
        (lovelaceToValue ll)
        TxOutDatumNone
        ReferenceScriptNone

data RunningNode = RunningNode
  { nodeSocket :: SocketPath
  , networkId :: NetworkId
  , blockTime :: NominalDiffTime
  -- ^ Expected time between blocks (varies a lot on testnets)
  }
  deriving (Show, Eq)
