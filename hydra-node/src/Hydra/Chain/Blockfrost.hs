module Hydra.Chain.Blockfrost where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.EpochInfo.API (EpochInfo, hoistEpochInfo)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  EraHistory (..),
  PaymentCredential (PaymentCredentialByKey),
  StakeAddressReference (NoStakeAddress),
  SystemStart (..),
  makeShelleyAddress,
  runExcept,
  toLedgerUTxO,
  verificationKeyHash,
 )
import Hydra.Chain.Blockfrost.Client (
  queryEraHistory,
  queryGenesisParameters,
  queryScriptRegistry,
  queryTip,
  queryUTxO,
  runBlockfrostM,
  toCardanoNetworkId,
  queryProtocolParameters,
 )
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.Direct.Handlers (
  DirectChainLog (..),
 )
import Hydra.Chain.Direct.State (ChainContext (..))
import Hydra.Chain.Direct.Wallet (TinyWallet, WalletInfoOnChain (..), newTinyWallet)
import Hydra.Logging (Tracer)
import Hydra.Node.Util (
  readKeyPair,
 )
import Hydra.Chain.Direct.Wallet (TinyWallet (..), WalletInfoOnChain (..), newTinyWallet)
import Hydra.Logging (Tracer)
import Hydra.Options (BlockfrostChainConfig (..))
import Hydra.Tx (Party)
import Ouroboros.Consensus.HardFork.History qualified as Consensus

-- | Build the 'ChainContext' from a 'BlockfrostChainConfig and additional information.
loadChainContext ::
  BlockfrostChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  IO ChainContext
loadChainContext config party = do
  (vk, _) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    scriptRegistry <- queryScriptRegistry hydraScriptsTxId
    Blockfrost.Genesis
      { _genesisNetworkMagic
      } <-
      queryGenesisParameters
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    pure $
      ChainContext
        { networkId
        , ownVerificationKey = vk
        , ownParty = party
        , scriptRegistry
        }
 where
  BlockfrostChainConfig
    { projectPath
    , hydraScriptsTxId
    , cardanoSigningKey
    } = config

mkTinyWallet ::
  Tracer IO DirectChainLog ->
  BlockfrostChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet tracer config = do
  keyPair@(vk, _) <- readKeyPair cardanoSigningKey
  prj <- Blockfrost.projectFromFile projectPath
  runBlockfrostM prj $ do
    Blockfrost.Genesis{_genesisSystemStart, _genesisNetworkMagic} <- queryGenesisParameters
    let networkId = toCardanoNetworkId _genesisNetworkMagic
    let address = makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress
    eraHistory <- queryEraHistory
    let queryEpochInfo = pure $ toEpochInfo eraHistory
    -- NOTE: we don't need to provide address here since it is derived from the
    -- keypair but we still want to keep the same wallet api.
    let queryWalletInfo queryPoint _address = runBlockfrostM prj $ do
          point <- case queryPoint of
            QueryAt point -> pure point
            QueryTip -> queryTip
          utxo <- queryUTxO [address]
          let walletUTxO = Ledger.unUTxO $ toLedgerUTxO utxo
          let systemStart = SystemStart $ posixSecondsToUTCTime _genesisSystemStart
          pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}
    let querySomePParams = runBlockfrostM prj queryProtocolParameters
    liftIO $ newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  BlockfrostChainConfig{projectPath, cardanoSigningKey} = config

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
