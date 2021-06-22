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
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import qualified Hydra.Contract.OffChain as OffChain
import qualified Hydra.Contract.OnChain as OnChain
import Ledger (MonetaryPolicy, MonetaryPolicyHash, PubKeyHash, TxOut, TxOutRef, TxOutTx, monetaryPolicyHash, pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (UtxoMap, outputsMapFromTxForAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Plutus.Contract (BlockchainActions, Contract, ContractError, Empty, logInfo, nextTransactionsAt, ownPubKey, tell, utxoAt, waitNSlots)
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
    let fn = "/tmp/W" <> show (getWallet w) <> ".cid"
    liftIO $ writeFile fn $ show cid
    pure fn

  cleanupWallets = mapM_ (liftIO . removeFile)

data PABContract
  = HydraContract
  | GetUtxos
  | WatchInit
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
    HydraContract -> Builtin.endpointsToSchemas @(OffChain.Schema .\\ BlockchainActions)
    GetUtxos -> endpointsToSchemas @Empty
    WatchInit -> endpointsToSchemas @Empty
  getContract = \case
    HydraContract -> SomeBuiltin hydraContract
    GetUtxos -> SomeBuiltin getUtxo
    WatchInit -> SomeBuiltin watchInit

hydraContract :: Contract [OnChain.State] OffChain.Schema ContractError ()
hydraContract = OffChain.contract headParameters
 where
  -- TODO(SN): Do not hard-code headParameters
  headParameters :: OffChain.HeadParameters
  headParameters = OffChain.mkHeadParameters [vk alice, vk bob] testPolicy

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
