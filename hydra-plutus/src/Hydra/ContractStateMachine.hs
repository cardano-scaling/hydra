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
  MonetaryPolicyHash ->
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition policy s i = case (s, i) of
  (state@State{stateData = Started}, Init vks) ->
    Just
      ( foldMap mustForgeParticipationToken vks
      , state{stateData = Initial vks []}
      )
  (state@State{stateData = Initial vks utxo}, Commit vk refs) ->
    case vks of
      (h : q)
        | h == vk ->
          Just
            ( foldMap mustLockUtxo refs <> mustForwardParticipationToken vk
            , state{stateData = Initial q (refs ++ utxo)}
            )
      _ ->
        Nothing
  (state@State{stateData = Initial vks utxo}, CollectCom) ->
    case vks of
      [] ->
        Just
          ( foldMap mustForwardParticipationToken vks
          , state{stateData = Open utxo}
          )
      _ ->
        Nothing
  (state@State{stateData = Open{}}, Close) ->
    Just
      ( mempty
      , state{stateData = Closed}
      )
  (_, _) -> Nothing
 where
  mkTokenName :: PubKeyHash -> TokenName
  mkTokenName = TokenName . getPubKeyHash

  mustForgeParticipationToken :: PubKeyHash -> TxConstraints i o
  mustForgeParticipationToken vk =
    let value = Value.singleton (Value.mpsSymbol policy) (mkTokenName vk) 1
     in mconcat
          [ Constraints.mustForgeCurrency policy (mkTokenName vk) 1
          , -- TODO: This doesn't account for _abort_. The token should be sent to
            -- an output where the validator allows either:
            --
            -- - An abort
            -- - A commit
            --
            --
            Constraints.mustPayToPubKey vk value
          ]

  mustLockUtxo :: (TxOutRef, TxOut) -> TxConstraints i o
  mustLockUtxo (ref, out) =
    let value = txOutValue out
     in mconcat
          [ Constraints.mustSpendPubKeyOutput ref
          , Constraints.mustProduceAtLeast value
          ]

  mustForwardParticipationToken :: PubKeyHash -> TxConstraints i o
  mustForwardParticipationToken vk =
    let value = Value.singleton (Value.mpsSymbol policy) (mkTokenName vk) 1
     in mconcat
          [ Constraints.mustSpendAtLeast value
          , Constraints.mustProduceAtLeast value
          ]

{-# INLINEABLE machine #-}
machine :: MonetaryPolicyHash -> SM.StateMachine HydraState HydraInput
machine policy = SM.mkStateMachine (transition policy) isFinal
 where
  isFinal _ = False

{-# INLINEABLE validatorSM #-}
validatorSM ::
  MonetaryPolicyHash ->
  Scripts.ValidatorType (SM.StateMachine HydraState HydraInput)
validatorSM = SM.mkValidator . machine

{- ORMOLU_DISABLE -}
contractInstance
  :: MonetaryPolicyHash
  -> Scripts.ScriptInstance (SM.StateMachine HydraState HydraInput)
contractInstance policy = Scripts.validator @(SM.StateMachine HydraState HydraInput)
    ($$(PlutusTx.compile [|| validatorSM ||]) `PlutusTx.applyCode` PlutusTx.liftCode policy)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

-- | The 'SM.StateMachineInstance' of the hydra state machine contract. It uses
-- the functions in 'PlutusTx.StateMachine'.
machineInstance :: MonetaryPolicyHash -> SM.StateMachineInstance HydraState HydraInput
machineInstance policy =
  SM.StateMachineInstance (machine policy) (contractInstance policy)

client :: MonetaryPolicyHash -> SM.StateMachineClient HydraState HydraInput
client policy =
  SM.StateMachineClient (machineInstance policy) chooser
 where
  chooser ::
    HydraInput ->
    [SM.OnChainState HydraState HydraInput] ->
    Either SM.SMContractError (SM.OnChainState HydraState HydraInput)
  chooser input states =
    let matchState =
          case input of
            Init{} -> pure
            Commit{} -> pure
            CollectCom{} -> pure
            Close{} -> pure
     in case mapMaybe matchState states of
          [state] -> Right state
          _ -> Left $ SM.ChooserError "Unable to choose SM state."

-- | The validator script of the contract.
contractValidator :: MonetaryPolicyHash -> Validator
contractValidator = Scripts.validatorScript . contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: MonetaryPolicyHash -> Address
contractAddress = Ledger.scriptAddress . contractValidator

mkClient :: HeadParameters -> SM.StateMachineClient HydraState HydraInput
mkClient = client . monetaryPolicyHash . monetaryPolicy

setupEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
setupEndpoint params = do
  endpoint @"setup" @()
  logInfo @String $ "setupEndpoint"
  void $ SM.runInitialise (mkClient params) Started (Ada.lovelaceValueOf 1)

-- | Our mocked "init" endpoint
initEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
initEndpoint params = do
  endpoint @"init" @()
  logInfo @String $ "initEndpoint"
  let policy = monetaryPolicy params
  let input = Init (verificationKeys params)
  void $ SM.runStepWith (mkClient params) input (withKnownPolicy policy)
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
  HeadParameters ->
  Contract () Schema e ()
collectComEndpoint params = do
  endpoint @"collectCom" @()
  logInfo @String $ "collectComEndpoint"
  void $ SM.runStep (mkClient params) CollectCom

-- | Our "close" endpoint to trigger a close
closeEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
closeEndpoint params = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  void $ SM.runStep (mkClient params) Close

commitEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
commitEndpoint params = do
  (vk, utxo) <- endpoint @"commit" @(PubKeyHash, UtxoMap)
  logInfo @String "commitEndpoint"
  void $ SM.runStepWith (mkClient params) (Commit vk $ toList' utxo) (withKnownUtxo utxo)
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
    setupEndpoint params
      `select` initEndpoint params
      `select` collectComEndpoint params
      `select` commitEndpoint params
      `select` closeEndpoint params
