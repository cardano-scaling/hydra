module Hydra.Chain.Blockfrost.Wallet where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Api (
  Conway,
  PParams,
 )
import Cardano.Ledger.Shelley.API (unUTxO)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.EpochInfo (EpochInfo)
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Hydra.Cardano.Api (
  NetworkId,
  PaymentCredential (..),
  PaymentKey,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  VerificationKey,
  fromLedgerTx,
  fromLedgerTxIn,
  fromLedgerUTxO,
  makeShelleyAddress,
  shelleyAddressInEra,
  toLedgerAddr,
  toLedgerTx,
  toLedgerUTxO,
  verificationKeyHash,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.Blockfrost.Client (getChainPoint, toCardanoNetworkId)
import Hydra.Chain.Direct.Wallet (applyTxs, coverFee_, findLargestUTxO)
import Hydra.Chain.Wallet (
  TinyWallet (..),
  TinyWalletLog (..),
  WalletInfoOnChain (..),
 )
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)

-- | Create a new tiny wallet handle.
newTinyWallet ::
  -- | A tracer for logging
  Tracer IO TinyWalletLog ->
  -- | The genesis information of the network.
  Blockfrost.Genesis ->
  -- | Credentials of the wallet.
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | A function to query for 'WalletInfo'
  (NetworkId -> IO WalletInfoOnChain) ->
  EpochInfo (Either Text) ->
  -- | A means to query some pparams.
  IO (PParams Conway) ->
  IO (TinyWallet IO)
newTinyWallet tracer genesis (vk, sk) queryWalletInfo queryEpochInfo querySomePParams = do
  walletInfoVar <- newTVarIO =<< initialize
  let getUTxO = readTVar walletInfoVar <&> walletUTxO
  pure
    TinyWallet
      { getUTxO
      , getSeedInput = fmap (fromLedgerTxIn . fst) . findLargestUTxO <$> getUTxO
      , sign = Api.signTx sk
      , coverFee = \lookupUTxO partialTx -> do
          let ledgerLookupUTxO = unUTxO $ toLedgerUTxO lookupUTxO
          WalletInfoOnChain{walletUTxO, systemStart} <- readTVarIO walletInfoVar
          let epochInfo = queryEpochInfo
          -- We query pparams here again as it's possible that a hardfork
          -- occurred and the pparams changed.
          pparams <- querySomePParams
          pure $
            fromLedgerTx
              <$> coverFee_ pparams systemStart epochInfo ledgerLookupUTxO walletUTxO (toLedgerTx partialTx)
      , reset = initialize >>= atomically . writeTVar walletInfoVar
      , update = \header txs -> do
          point <- getChainPoint header
          walletTip <- atomically $ readTVar walletInfoVar <&> \WalletInfoOnChain{tip} -> tip
          if point < walletTip
            then traceWith tracer $ SkipUpdate{point}
            else do
              traceWith tracer $ BeginUpdate{point}
              utxo' <- atomically $ do
                walletInfo@WalletInfoOnChain{walletUTxO} <- readTVar walletInfoVar
                let utxo' = applyTxs txs (== ledgerAddress) walletUTxO
                writeTVar walletInfoVar $ walletInfo{walletUTxO = utxo', tip = point}
                pure utxo'
              traceWith tracer $ EndUpdate (fromLedgerUTxO (Ledger.UTxO utxo'))
      }
 where
  Blockfrost.Genesis{_genesisNetworkMagic, _genesisSystemStart} = genesis

  initialize = do
    traceWith tracer BeginInitialize
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    walletInfo@WalletInfoOnChain{walletUTxO, tip} <- queryWalletInfo networkId
    traceWith tracer $ EndInitialize{initialUTxO = fromLedgerUTxO (Ledger.UTxO walletUTxO), tip}
    pure walletInfo

  address =
    let networkId = toCardanoNetworkId _genesisNetworkMagic
     in makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress

  ledgerAddress = toLedgerAddr $ shelleyAddressInEra @Api.Era Api.shelleyBasedEra address
