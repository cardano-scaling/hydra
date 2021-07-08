{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Prelude hiding (log)

import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import qualified Hydra.Contract.OnChain as OnChain
import Hydra.Contract.PAB (PABContract (..))
import qualified Hydra.ContractSM as ContractSM
import Ledger (MonetaryPolicy, MonetaryPolicyHash, PubKeyHash, TxOut, TxOutRef, TxOutTx, monetaryPolicyHash, pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract (BlockchainActions, Contract, ContractError, Empty, logInfo, ownPubKey, tell, utxoAt, waitNSlots)
import Plutus.Contract.Test (walletPubKey)
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Schema (FormSchema (..), ToSchema (..))
import Wallet.Emulator.Types (Wallet (..))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    log "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    log "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContract) b

    shutdown
 where
  log = Simulator.logString @(Builtin PABContract)

handlers :: SimulatorEffectHandlers (Builtin PABContract)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin PABContract) [] $
    interpret handleStarterContract

handleStarterContract ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin PABContract))) effs
  ) =>
  ContractEffect (Builtin PABContract)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract
 where
  getSchema = \case
    -- NOTE: There are actual endpoints defined in contracts  code but they
    -- are not exposed here -> "different" schemas can exist between the "server"
    -- side code (off-chain contracts) and what the PAB webserver exposes
    Setup -> endpointsToSchemas @Empty
    GetUtxos -> endpointsToSchemas @Empty
    WatchInit -> endpointsToSchemas @Empty
  getContract = \case
    Setup -> SomeBuiltin ContractSM.setup
    GetUtxos -> SomeBuiltin getUtxo
    WatchInit -> SomeBuiltin ContractSM.watchInit

getUtxo :: Contract (Last UtxoMap) BlockchainActions ContractError ()
getUtxo = do
  logInfo @Text $ "getUtxo: Starting to get and report utxo map every slot"
  address <- pubKeyAddress <$> ownPubKey
  loop address
 where
  loop address = do
    utxos <- utxoAt address
    tell . Last $ Just utxos
    void $ waitNSlots 1
    loop address

testPolicy :: MonetaryPolicy
testPolicy = OnChain.hydraMonetaryPolicy 42

testPolicyId :: MonetaryPolicyHash
testPolicyId = monetaryPolicyHash testPolicy

vk :: Wallet -> PubKeyHash
vk = pubKeyHash . walletPubKey

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
