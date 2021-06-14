{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Cardano.Prelude

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON (..),
  Options (..),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import GHC.Generics (Generic)
import Hydra.Contract.OffChain as OffChain
import Hydra.Contract.OnChain as OnChain
import Ledger (MonetaryPolicy, pubKeyHash, TxOut, TxOutRef, TxOutTx)
import Plutus.Contract (Contract, BlockchainActions, ContractError, Empty, logInfo)
import Plutus.Contract.Test (walletPubKey)
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (PABError (..))
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Wallet.Emulator.Types (Wallet (..))
import Schema (ToSchema(..), FormSchema(..))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PABContract) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- TODO(SN): Use 'FundWallets' here instead of "setupForTesting" endpoint to
    -- distribute initial funds

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin PABContract) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContract) b

    shutdown

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
    HydraContract -> SomeBuiltin (OffChain.contract @ContractError headParameters)

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
headParameters = mkHeadParameters [vk alice, vk bob] testPolicy
 where
  vk = pubKeyHash . walletPubKey

  alice :: Wallet
  alice = Wallet 1

  bob :: Wallet
  bob = Wallet 2

  testPolicy :: MonetaryPolicy
  testPolicy = OnChain.hydraMonetaryPolicy 42

-- REVIEW(SN): Orphan ToSchema instances, required to render the playground? Do
-- we really need all these as endpoint parameters?

instance ToSchema TxOut where
  toSchema = FormSchemaUnsupported "TxOut"

instance ToSchema TxOutRef where
  toSchema = FormSchemaUnsupported "TxOutRef"

instance ToSchema TxOutTx where
  toSchema = FormSchemaUnsupported "TxOutTx"
