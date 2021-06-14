{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Prelude hiding (log)

import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
 )
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import qualified Hydra.Contract.OffChain as OffChain
import qualified Hydra.Contract.OnChain as OnChain
import Ledger (MonetaryPolicy, TxOut, TxOutRef, TxOutTx, pubKeyHash)
import Plutus.Contract (BlockchainActions, Contract, ContractError, Empty, logInfo)
import Plutus.Contract.Test (walletPubKey)
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Schema (FormSchema (..), ToSchema (..))
import System.Directory (removeFile)
import Wallet.Emulator.Types (Wallet (..))
import Wallet.Types (ContractInstanceId (ContractInstanceId))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    log "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- Activate wallets and write contract instances into files
    files <- activateWallets

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    log "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContract) b

    -- Best-effort cleanup, due to the lack of a lifted bracket
    cleanupWallets files
    shutdown
 where
  log = Simulator.logString @(Builtin PABContract)

  activateWallets = forM [alice, bob] $ \w -> do
    (ContractInstanceId cid) <- Simulator.activateContract w HydraContract
    let fn = 'W' : show (getWallet w) <> ".cid"
    liftIO $ writeFile fn $ show cid
    pure fn

  cleanupWallets = mapM_ (liftIO . removeFile)

data PABContract
  = FundWallets
  | HydraContract
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow

handleStarterContract ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin PABContract))) effs
  ) =>
  ContractEffect (Builtin PABContract)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract
 where
  getSchema = \case
    FundWallets -> endpointsToSchemas @Empty
    HydraContract -> Builtin.endpointsToSchemas @(OffChain.Schema .\\ BlockchainActions)
  getContract = \case
    FundWallets -> SomeBuiltin fundWallets
    HydraContract -> SomeBuiltin hydraContract

hydraContract :: Contract [OnChain.State] OffChain.Schema ContractError ()
hydraContract = OffChain.contract headParameters

fundWallets :: Contract () BlockchainActions ContractError ()
fundWallets =
  logInfo @Text $ "TODO: Should distribute funds"

handlers :: SimulatorEffectHandlers (Builtin PABContract)
handlers =
  -- REVIEW(SN): only HydraContract required here?
  Simulator.mkSimulatorHandlers @(Builtin PABContract) [HydraContract] $
    interpret handleStarterContract

-- TODO(SN): Do not hard-code headParameters
headParameters :: OffChain.HeadParameters
headParameters = OffChain.mkHeadParameters [vk alice, vk bob] testPolicy
 where
  vk = pubKeyHash . walletPubKey

  testPolicy :: MonetaryPolicy
  testPolicy = OnChain.hydraMonetaryPolicy 42

-- TODO(SN): Do not hard-code wallets
alice :: Wallet
alice = Wallet 1

bob :: Wallet
bob = Wallet 2

-- REVIEW(SN): Orphan ToSchema instances, required to render the playground? Do
-- we really need all these as endpoint parameters?

instance ToSchema TxOut where
  toSchema = FormSchemaUnsupported "TxOut"

instance ToSchema TxOutRef where
  toSchema = FormSchemaUnsupported "TxOutRef"

instance ToSchema TxOutTx where
  toSchema = FormSchemaUnsupported "TxOutTx"
