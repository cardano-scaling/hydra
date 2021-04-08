{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.ContractStateMachine where

import Control.Monad (forever, guard, void)
import Ledger (Address, PubKeyHash (..), Validator, Value, scriptAddress)
import qualified Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

import Hydra.Contract ()
import Hydra.Contract.Types

{-# INLINEABLE transition #-}
transition :: State HydraState -> HydraInput -> Maybe (SM.TxConstraints Void Void, State HydraState)
transition state@State{stateData = Initial} (Init params) =
  -- NOTE: vvv maybe we can add constraints for forging PTs here?
  Just
    ( foldMap constraints $ verificationKeys params
    , state{stateData = Collecting}
    )
 where
  constraints vk =
    let val =
          Value.singleton
            (currencyId params)
            (TokenName $ getPubKeyHash vk)
            1
     in Constraints.mustForgeValue val <> Constraints.mustPayToPubKey vk val
transition state@State{stateData = Collecting} CollectCom =
  -- NOTE: vvv maybe we can add constraints for collecting PTs here?
  Just (mempty, state{stateData = Open openState})
 where
  openState =
    OpenState
      { keyAggregate = MultisigPublicKey []
      , eta = Eta UTXO 0 []
      }
transition state@State{stateData = Open OpenState{eta, keyAggregate}} (Close xi) =
  case close keyAggregate eta xi of
    Just{} -> Just (mempty, state{stateData = Closed})
    Nothing -> Nothing
transition _ _ = Nothing

{-# INLINEABLE close #-}
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close kAgg eta xi = do
  let (Xi u s sigma txs) = xi
  guard (all (verifyMultisignature kAgg) txs)
  guard (s == 0 || verifySnapshot kAgg u s sigma)
  let realU =
        if s == 0
          then utxos eta
          else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

{-# INLINEABLE verifyMultisignature #-}
verifyMultisignature :: MultisigPublicKey -> TransactionObject -> Bool
verifyMultisignature kAgg TransactionObject{sigma, tx} =
  msAVerify kAgg (hash $ serialize tx) sigma

{-# INLINEABLE verifySnapshot #-}
verifySnapshot :: MultisigPublicKey -> UTXO -> Integer -> MultiSignature -> Bool
verifySnapshot kAgg u s sigma =
  msAVerify kAgg (hash $ serialize u <> serialize s) sigma

-- | This is only about folding the transactions onto a UTXO and no evaluation
-- whatsoever.
applyTransactions :: UTXO -> [Transaction] -> Maybe UTXO
applyTransactions u _ = Just u -- TODO

--
-- Primitives we need
--

serialize :: a -> ByteString
serialize = const "reuse plutus tx's isData stuff" -- TODO

hash :: ByteString -> ByteString
hash = const "hashed bytestring" -- TODO

msAVerify :: MultisigPublicKey -> ByteString -> MultiSignature -> Bool
msAVerify _ _ _ = True -- TODO

--
-- Boilerplate
--

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

setupEndpoint :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
setupEndpoint = do
  endpoint @"setup" @()
  logInfo @String $ "setupEndpoint"
  void $ SM.runInitialise client initialState (Ada.lovelaceValueOf 1)
 where
  initialState = Initial

-- | Our mocked "init" endpoint
initEndpoint :: (AsContractError e, SM.AsSMContractError e) => HeadParameters -> Contract () Schema e ()
initEndpoint params = do
  endpoint @"init" @()
  logInfo @String $ "initEndpoint"
  void $ SM.runStep client input
 where
  input = Init params

data CollectComParams = CollectComParams
  { amount :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Our mocked "collectCom" endpoint
collectComEndpoint :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
collectComEndpoint = do
  CollectComParams _amt <- endpoint @"collectCom" @CollectComParams
  logInfo @String $ "collectComEndpoint"
  void $ SM.runStep client input
 where
  input = CollectCom

-- | Our "close" endpoint to trigger a close
closeEndpoint :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
closeEndpoint = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  void $ SM.runStep client input
 where
  input = Close $ Xi UTXO 0 MultiSignature []

type Schema =
  BlockchainActions
    .\/ Endpoint "setup" ()
    .\/ Endpoint "init" ()
    .\/ Endpoint "collectCom" CollectComParams
    .\/ Endpoint "close" ()

contract ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
contract params = forever endpoints
 where
  endpoints =
    setupEndpoint
      `select` initEndpoint params
      `select` collectComEndpoint
      `select` closeEndpoint
