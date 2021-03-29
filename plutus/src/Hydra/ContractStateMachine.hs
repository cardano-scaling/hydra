{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.ContractStateMachine where

import Control.Monad (guard, void)
import qualified Data.Map as Map
import Ledger (Address, Validator, ValidatorCtx, Value, scriptAddress)
import qualified Ledger.Constraints as Constraints
import Ledger.Tx (TxOut (..), TxOutTx (..))
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

import Hydra.Contract ()

data HydraState = Closed

data HydraInput = HydraInput

{-# INLINEABLE transition #-}
transition ::
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition State{stateData = oldData, stateValue = oldValue} input = Nothing

{-# INLINEABLE machine #-}
machine :: SM.StateMachine HydraState HydraInput
machine = SM.mkStateMachine transition isFinal where isFinal _ = False

{-# INLINEABLE validatorSM #-}
validatorSM :: Scripts.ValidatorType (SM.StateMachine HydraState HydraInput)
validatorSM = SM.mkValidator machine

{- ORMOLU_DISABLE -}
contractInstance
  :: Scripts.ScriptInstance (SM.StateMachine HydraState HydraInput)
contractInstance = Scripts.validator @(SM.StateMachine HydraState HydraInput)
    $$(PlutusTx.compile [|| validatorSM ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

-- | The 'SM.StateMachineInstance' of the hydra state machine contract. It uses
-- the functions in 'PlutusTx.StateMachine'.
machineInstance :: SM.StateMachineInstance HydraState HydraInput
machineInstance = SM.StateMachineInstance machine contractInstance

client :: SM.StateMachineClient HydraState HydraInput
client = SM.mkStateMachineClient machineInstance

-- | The validator script of the contract.
contractValidator :: Validator
contractValidator = Scripts.validatorScript contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: Address
contractAddress = Ledger.scriptAddress contractValidator

data CollectComParams = CollectComParams
  { amount :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Our mocked "collectCom" endpoint
collectComEndpoint :: AsContractError e => Contract () Schema e ()
collectComEndpoint = do
  CollectComParams amt <- endpoint @"collectCom" @CollectComParams
  logInfo @String $ "collectComEndpoint"

--  let tx = Constraints.mustPayToTheScript datum amt
--  void (submitTxConstraints contractInstance tx)
-- where
--  datum =
--    Open $ OpenState { keyAggregate = MultisigPublicKey, eta = Eta UTXO 0 [] }

-- | Our "close" endpoint to trigger a close
closeEndpoint :: AsContractError e => Contract () Schema e ()
closeEndpoint = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"

-- Querying ledger from application backend
--  utxoMap <- utxoAt contractAddress
--  let balance = foldMap (txOutValue . txOutTxOut . snd) $ Map.toList utxoMap
--  logInfo @String $ "CONTRACT BALANCE: " ++ show balance
--  let
--    tx =
--      collectFromScript utxoMap redeemer
--        <> Constraints.mustPayToTheScript datum balance
--  void (submitTxConstraintsSpending contractInstance utxoMap tx)
-- where
--  datum    = Closed -- TODO add more things
--  redeemer = Redeemer $ Xi UTXO 0 MultiSignature []

type Schema =
  BlockchainActions
    .\/ Endpoint "collectCom" CollectComParams
    .\/ Endpoint "close" ()

hydraHead :: AsContractError e => Contract () Schema e ()
hydraHead = collectComEndpoint `select` closeEndpoint -- TODO loop here?

--
-- Template Haskell
--

PlutusTx.unstableMakeIsData ''HydraState
PlutusTx.makeLift ''HydraState
PlutusTx.unstableMakeIsData ''HydraInput
PlutusTx.makeLift ''HydraInput
