{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Prelude hiding (log)

import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import qualified Data.Map as Map
import qualified Hydra.Contract.OnChain as OnChain
import Hydra.Contract.PAB (PABContract (..))
import Ledger (MonetaryPolicy, MonetaryPolicyHash, PubKeyHash, TxOut, TxOutRef, TxOutTx, monetaryPolicyHash, pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (UtxoMap, outputsMapFromTxForAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Plutus.Contract (BlockchainActions, Contract, ContractError, Empty, logInfo, nextTransactionsAt, ownPubKey, tell, utxoAt, waitNSlots)
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
import qualified Hydra.ContractSM as ContractSM

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

handleStarterContract ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin PABContract))) effs
  ) =>
  ContractEffect (Builtin PABContract)
    ~> Eff effs
handleStarterContract = Builtin.handleBuiltin getSchema getContract
 where
  getSchema = \case
    HydraContract -> endpointsToSchemas @Empty
    GetUtxos -> endpointsToSchemas @Empty
    WatchInit -> endpointsToSchemas @Empty
  getContract = \case
    HydraContract -> SomeBuiltin hydraContract
    GetUtxos -> SomeBuiltin getUtxo
    WatchInit -> SomeBuiltin watchInit

hydraContract :: Contract () BlockchainActions ContractSM.HydraPlutusError ()
hydraContract =
  -- NOTE(SN): This is obviously not what we want, as it does initialize a new
  -- hydra main chain state machine on every contract activation and we might
  -- want to use endpoints instead.
  ContractSM.init

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

-- | Watch 'initialAddress' (with hard-coded parameters) and report all datums
-- seen on each run.
watchInit :: Contract (Last [PubKeyHash]) BlockchainActions ContractError ()
watchInit = do
  logInfo @Text $ "watchInit: Looking for an init tx"
  forever $ do
    -- NOTE(SN): this is essentially 'Plutus.Contract.StateMachine.waitForUpdate'
    txs <- nextTransactionsAt initialAddress
    let datums = txs >>= rights . fmap lookupDatum . Map.elems . outputsMapFromTxForAddress initialAddress
    logInfo @Text $ "found init tx(s) with datums: " <> show datums
    tell . Last $ Just datums
    void $ waitNSlots 1 -- TODO(SN): really wait?
 where
  validator = OnChain.initialTypedValidator headParameters

  initialAddress = Scripts.validatorAddress validator

  lookupDatum txOutTx = tyTxOutData <$> typeScriptTxOut validator txOutTx

  -- TODO(SN): Do not hard-code headParameters
  headParameters = OnChain.HeadParameters [vk alice, vk bob] testPolicyId

handlers :: SimulatorEffectHandlers (Builtin PABContract)
handlers =
  -- REVIEW(SN): only HydraContract required here?
  Simulator.mkSimulatorHandlers @(Builtin PABContract) [HydraContract] $
    interpret handleStarterContract

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
