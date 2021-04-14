{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.ContractStateMachine where

import Control.Arrow (second)
import Control.Monad (forever, void)
import qualified Data.Map.Strict as Map
import Hydra.Contract.Types
import Ledger (
  Address,
  MonetaryPolicy,
  MonetaryPolicyHash,
  PubKeyHash (..),
  TxOut (..),
  TxOutTx (..),
  Validator,
  monetaryPolicyHash,
  scriptAddress,
 )
import qualified Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints.OffChain (ScriptLookups (..))
import Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

{-# INLINEABLE transition #-}
transition ::
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition s i = case (s, i) of
  (state@State{stateData = Started}, Init vks policy) ->
    Just
      ( foldMap (mustForgeParticipationToken policy) vks
      , state{stateData = Initial vks policy}
      )
  (state@State{stateData = Initial vks policy}, Commit vk refs) ->
    case vks of
      (h : q)
        | h == vk ->
          Just
            ( foldMap mustLockUtxo refs <> mustForwardParticipationToken policy vk
            , state{stateData = Initial q policy}
            )
      _ ->
        Nothing
  (state@State{stateData = Initial vks _}, CollectCom) ->
    case vks of
      [] ->
        Just
          ( mempty
          , state{stateData = Open}
          )
      _ ->
        Nothing
  (state@State{stateData = Open}, Close) ->
    Just
      ( mempty
      , state{stateData = Closed}
      )
  (_, _) -> Nothing
 where
  mkTokenName :: PubKeyHash -> TokenName
  mkTokenName = TokenName . getPubKeyHash

  mustForgeParticipationToken :: MonetaryPolicyHash -> PubKeyHash -> TxConstraints i o
  mustForgeParticipationToken policy vk =
    let value = Value.singleton (Value.mpsSymbol policy) (mkTokenName vk) 1
     in mconcat
          [ Constraints.mustForgeCurrency policy (mkTokenName vk) 1
          , Constraints.mustPayToPubKey vk value
          ]

  mustLockUtxo :: (TxOutRef, TxOut) -> TxConstraints i o
  mustLockUtxo (ref, out) =
    let value = txOutValue out
     in mconcat
          [ Constraints.mustSpendPubKeyOutput ref
          , Constraints.mustProduceAtLeast value
          ]

  mustForwardParticipationToken :: MonetaryPolicyHash -> PubKeyHash -> TxConstraints i o
  mustForwardParticipationToken policy vk =
    let value = Value.singleton (Value.mpsSymbol policy) (mkTokenName vk) 1
     in mconcat
          [ Constraints.mustSpendAtLeast value
          , Constraints.mustProduceAtLeast value
          ]

{-# INLINEABLE machine #-}
machine :: SM.StateMachine HydraState HydraInput
machine = SM.mkStateMachine transition isFinal
 where
  isFinal _ = False

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

setupEndpoint ::
  (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
setupEndpoint = do
  endpoint @"setup" @()
  logInfo @String $ "setupEndpoint"
  void $ SM.runInitialise client Started (Ada.lovelaceValueOf 1)

-- | Our mocked "init" endpoint
initEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
initEndpoint params = do
  endpoint @"init" @()
  logInfo @String $ "initEndpoint"
  let policy = monetaryPolicy params
  let input = Init (verificationKeys params) (monetaryPolicyHash policy)
  void $ SM.runStepWith client input (withKnownPolicy policy)
 where
  withKnownPolicy ::
    MonetaryPolicy ->
    SM.StateMachineTransition state input ->
    SM.StateMachineTransition state input
  withKnownPolicy policy sm@SM.StateMachineTransition{SM.smtLookups} =
    sm
      { SM.smtLookups =
          smtLookups
            { slMPS =
                Prelude.mconcat
                  [ slMPS smtLookups
                  , Map.fromList [(monetaryPolicyHash policy, policy)]
                  ]
            }
      }

-- | Our mocked "collectCom" endpoint
collectComEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  Contract () Schema e ()
collectComEndpoint = do
  endpoint @"collectCom" @()
  logInfo @String $ "collectComEndpoint"
  void $ SM.runStep client CollectCom

-- | Our "close" endpoint to trigger a close
closeEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  Contract () Schema e ()
closeEndpoint = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  void $ SM.runStep client Close

commitEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  Contract () Schema e ()
commitEndpoint = do
  (vk, utxo) <- endpoint @"commit" @(PubKeyHash, UtxoMap)
  logInfo @String "commitEndpoint"
  void $ SM.runStepWith client (Commit vk $ toList' utxo) (withKnownUtxo utxo)
 where
  toList' :: UtxoMap -> [(TxOutRef, TxOut)]
  toList' =
    fmap (second txOutTxOut) . Map.assocs

  withKnownUtxo ::
    UtxoMap ->
    SM.StateMachineTransition state input ->
    SM.StateMachineTransition state input
  withKnownUtxo utxo sm@SM.StateMachineTransition{SM.smtLookups} =
    sm
      { SM.smtLookups =
          smtLookups
            { slTxOutputs =
                slTxOutputs smtLookups Prelude.<> utxo
            }
      }

type Schema =
  BlockchainActions
    .\/ Endpoint "setup" ()
    .\/ Endpoint "init" ()
    .\/ Endpoint "collectCom" ()
    .\/ Endpoint "close" ()
    .\/ Endpoint "commit" (PubKeyHash, UtxoMap)

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
      `select` commitEndpoint
      `select` closeEndpoint
