module Hydra.Chain.Blockfrost.Wallet where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Api (
  Conway,
  PParams,
 )
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.EpochInfo (EpochInfo)
import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.Cardano.Api (
  NetworkId,
  PaymentKey,
  SigningKey,
  VerificationKey,
  fromLedgerUTxO,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.Blockfrost.Client (textAddrOf, toCardanoNetworkId)
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
  IO (EpochInfo (Either Text)) ->
  -- | A means to query some pparams.
  IO (PParams Conway) ->
  IO (TinyWallet IO)
newTinyWallet tracer genesis (vk, sk) queryWalletInfo queryEpochInfo querySomePParams = do
  walletInfoVar <- newTVarIO =<< initialize
  let getUTxO = readTVar walletInfoVar <&> walletUTxO
  pure
    TinyWallet
      { getUTxO
      , getSeedInput = undefined
      , sign = Api.signTx sk
      , coverFee = \_ -> undefined
      , reset = initialize >>= atomically . writeTVar walletInfoVar
      , update = \_ -> undefined
      }
 where
  Blockfrost.Genesis{_genesisNetworkMagic, _genesisSystemStart} = genesis
  initialize = do
    traceWith tracer BeginInitialize
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    walletInfo@WalletInfoOnChain{walletUTxO, tip} <- queryWalletInfo networkId
    traceWith tracer $ EndInitialize{initialUTxO = fromLedgerUTxO (Ledger.UTxO walletUTxO), tip}
    pure walletInfo
