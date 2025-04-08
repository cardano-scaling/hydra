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
  genesis <- runBlockfrostM prj queryGenesis
  newTinyWallet (contramap Wallet tracer) genesis keyPair (queryWalletInfo prj sk) queryEpochInfo querySomePParams
 where
  BlockfrostChainConfig{projectPath, cardanoSigningKey} = config

  queryEpochInfo = undefined

  querySomePParams = undefined

  queryWalletInfo prj sk networkId = runBlockfrostM prj $ do
    point <- queryTip
    bfUTxO <- queryUTxO sk networkId
    let walletUTxO = Ledger.unUTxO $ toLedgerUTxO bfUTxO
    systemStart <- querySystemStart
    pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}
