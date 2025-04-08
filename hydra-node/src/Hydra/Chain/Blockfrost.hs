module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.EpochInfo.API (EpochInfo, hoistEpochInfo)
import Hydra.Cardano.Api (
  EraHistory (..),
  runExcept,
  toLedgerUTxO,
 )
import Hydra.Chain.Blockfrost.Client (
  mkEraHistory,
  queryGenesis,
  querySystemStart,
  queryTip,
  queryUTxO,
  runBlockfrostM,
  toCardanoPParams,
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
import Ouroboros.Consensus.HardFork.History qualified as Consensus

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  BlockfrostChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair@(_, sk) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  genesis <- runBlockfrostM prj queryGenesis
  let querySomePParams = runBlockfrostM prj toCardanoPParams
  newTinyWallet (contramap Wallet tracer) genesis keyPair (queryWalletInfo prj sk) (queryEpochInfo genesis) querySomePParams
 where
  BlockfrostChainConfig{projectPath, cardanoSigningKey} = config

  queryEpochInfo genesis = toEpochInfo $ mkEraHistory genesis

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

  queryWalletInfo prj sk networkId = runBlockfrostM prj $ do
    point <- queryTip
    utxo <- queryUTxO sk networkId
    let walletUTxO = Ledger.unUTxO $ toLedgerUTxO utxo
    systemStart <- querySystemStart
    pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}
