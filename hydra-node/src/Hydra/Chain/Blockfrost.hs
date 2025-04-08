module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Shelley.API qualified as Ledger
import Hydra.Cardano.Api (
  toLedgerUTxO,
 )
import Hydra.Chain.Blockfrost.Client (
  queryGenesis,
  querySystemStart,
  queryTip,
  queryUTxO,
  runBlockfrostM,
 )
import Hydra.Chain.Blockfrost.Wallet (newTinyWallet)
import Hydra.Chain.Direct.Handlers (
  DirectChainLog (..),
 )
import Hydra.Chain.Direct.Util (
  readKeyPair,
 )
import Hydra.Chain.Wallet (
  TinyWallet (..),
  WalletInfoOnChain (..),
 )
import Hydra.Logging (Tracer)
import Hydra.Options (BlockfrostChainConfig (..))

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  BlockfrostChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair@(_, sk) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    Blockfrost.Genesis{_genesisSystemStart, _genesisNetworkMagic} <- queryGenesis
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    eraHistory <- mkEraHistory
    let queryEpochInfo = pure $ toEpochInfo eraHistory
    -- NOTE: we don't need to provide address here since it is derived from the
    -- keypair but we still want to keep the same wallet api.
    let queryWalletInfo queryPoint _address = runBlockfrostM prj $ do
          point <- queryTip queryPoint
          utxo <- queryUTxO sk networkId
          let walletUTxO = Ledger.unUTxO $ toLedgerUTxO utxo
          let systemStart = SystemStart $ posixSecondsToUTCTime _genesisSystemStart
          pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}
    let querySomePParams = runBlockfrostM prj toCardanoPParams
    liftIO $ newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  BlockfrostChainConfig{projectPath, cardanoSigningKey} = config

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
