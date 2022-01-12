{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | On-chain code fragments for creating a state machine. First
--   define a @StateMachine s i@ with input type @i@ and state type @s@. Then
--   use 'mkValidator' in on-chain code to check the required hashes and
--   validate the transition, and 'mkRedeemer' to make redeemer scripts.
--
--  ''NOTE'': Vendored from https://github.com/input-output-hk/plutus-apps/tree/d0fd9d49e6e862dc5abed41f0f07f56aafb652cf/plutus-contract/src/Plutus/Contract/StateMachine/OnChain.hs
-- NOTE: This has since deviated as we pass the script context to the 'smTransition' function!
module Plutus.Contract.StateMachine.OnChain (
  StateMachine (..),
  StateMachineInstance (..),
  State (..),
  machineAddress,
  mkValidator,
  threadTokenValueOrZero,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Address, ValidatorHash)
import Ledger.Constraints
import Ledger.Constraints.TxConstraints (OutputConstraint (..))
import Ledger.Contexts (ScriptContext (..), TxInInfo (..), findOwnInput, ownHash)
import Ledger.Tx (TxOut (..))
import Ledger.Typed.Scripts
import Ledger.Value (Value, isZero)
import qualified PlutusTx
import PlutusTx.Prelude hiding (check)
import qualified Prelude as Haskell

import qualified Plutus.Contract.StateMachine.ThreadToken as TT

data State s = State {stateData :: s, stateValue :: Value}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Specification of a state machine, consisting of a transition function that determines the
-- next state from the current state and an input, and a checking function that checks the validity
-- of the transition in the context of the current transaction.
data StateMachine s i = StateMachine
  { -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
    smTransition :: ScriptContext -> State s -> i -> Maybe (TxConstraints Void Void, State s)
  , -- | Check whether a state is the final state
    smFinal :: s -> Bool
  , -- | The condition checking function. Can be used to perform
    --   checks on the pending transaction that aren't covered by the
    --   constraints. 'smCheck' is always run in addition to checking the
    --   constraints, so the default implementation always returns true.
    smCheck :: s -> i -> ScriptContext -> Bool
  , -- | The 'ThreadToken' that identifies the contract instance.
    --   Make one with 'getThreadToken' and pass it on to 'mkStateMachine'.
    --   Initialising the machine will then mint a thread token value.
    smThreadToken :: Maybe TT.ThreadToken
  }

{-# INLINEABLE threadTokenValueInner #-}
threadTokenValueInner :: Maybe TT.ThreadToken -> ValidatorHash -> Value
threadTokenValueInner = maybe (const mempty) (TT.threadTokenValue . TT.ttCurrencySymbol)

{-# INLINEABLE threadTokenValueOrZero #-}

-- | The 'Value' containing exactly the thread token, if one has been specified.
threadTokenValueOrZero :: StateMachineInstance s i -> Value
threadTokenValueOrZero StateMachineInstance{stateMachine, typedValidator} =
  threadTokenValueInner (smThreadToken stateMachine) (validatorHash typedValidator)

instance ValidatorTypes (StateMachine s i) where
  type RedeemerType (StateMachine s i) = i
  type DatumType (StateMachine s i) = s

data StateMachineInstance s i = StateMachineInstance
  { -- | The state machine specification.
    stateMachine :: StateMachine s i
  , -- | The validator code for this state machine.
    typedValidator :: TypedValidator (StateMachine s i)
  }

machineAddress :: StateMachineInstance s i -> Address
machineAddress = validatorAddress . typedValidator

{-# INLINEABLE mkValidator #-}

-- | Turn a state machine into a validator script.
mkValidator :: forall s i. (PlutusTx.ToData s) => StateMachine s i -> ValidatorType (StateMachine s i)
mkValidator (StateMachine step isFinal check threadToken) currentState input ctx =
  let vl = maybe (traceError "S0" {-"Can't find validation input"-}) (txOutValue . txInInfoResolved) (findOwnInput ctx)
      checkOk =
        traceIfFalse "S1" {-"State transition invalid - checks failed"-} (check currentState input ctx)
          && traceIfFalse "S2" {-"Thread token not found"-} (TT.checkThreadToken threadToken (ownHash ctx) vl 1)
      oldState =
        State
          { stateData = currentState
          , -- The thread token value is hidden from the client code
            stateValue = vl <> inv (threadTokenValueInner threadToken (ownHash ctx))
          }
      stateAndOutputsOk = case step ctx oldState input of
        Just (newConstraints, State{stateData = newData, stateValue = newValue})
          | isFinal newData ->
            traceIfFalse "S3" {-"Non-zero value allocated in final state"-} (isZero newValue)
              && traceIfFalse "S4" {-"State transition invalid - constraints not satisfied by ScriptContext"-} (checkScriptContext newConstraints ctx)
          | otherwise ->
            let txc =
                  newConstraints
                    { txOwnOutputs =
                        [ OutputConstraint
                            { ocDatum = newData
                            , -- Check that the thread token value is still there
                              ocValue = newValue <> threadTokenValueInner threadToken (ownHash ctx)
                            }
                        ]
                    }
             in traceIfFalse "S5" {-"State transition invalid - constraints not satisfied by ScriptContext"-} (checkScriptContext @_ @s txc ctx)
        Nothing -> trace "S6" {-"State transition invalid - input is not a valid transition at the current state"-} False
   in checkOk && stateAndOutputsOk
