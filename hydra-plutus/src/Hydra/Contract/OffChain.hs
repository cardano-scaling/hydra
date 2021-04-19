{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OffChain where

import PlutusTx.Prelude hiding (init)

import Control.Monad (
  void,
 )
import Ledger
import Ledger.Ada (lovelaceValueOf)
import Ledger.Typed.Scripts (ScriptType (DatumType, RedeemerType))
import Ledger.Value (TokenName (..))
import Plutus.Contract (
  AsContractError,
  BlockchainActions,
  Contract,
  Endpoint,
  endpoint,
  select,
  submitTxConstraints,
  type (.\/),
 )
import PlutusTx.IsData.Class (IsData (..))

import qualified Hydra.Contract.OnChain as OnChain
import qualified Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx

--
-- Initial
--

-- The initial transaction has `n` outputs, where each output is locked by a
-- validator 'νinitial' and the i-th output has k_i in its data field.
-- The validator 'νinitial' ensures the following:
--   - either the output is consumed by an abort transaction, or
--   - it is consumed by a commit transaction.
--
-- It also initialises the state-machine,
init ::
  (AsContractError e) =>
  MonetaryPolicyHash ->
  Contract () Schema e ()
init policyId = do
  vks <- endpoint @"init" @[PubKeyHash]
  let state = OnChain.Initial
  let constraints =
        mconcat
          [ foldMap
              ( \vk ->
                  let participationToken = OnChain.mkParticipationToken policyId vk
                   in mconcat
                        [ Constraints.mustPayToOtherScript
                            (OnChain.νInitialHash policyId)
                            (OnChain.ρInitial vk)
                            participationToken
                        , Constraints.mustForgeValue
                            participationToken
                        ]
              )
              vks
          , Constraints.mustPayToTheScript state (lovelaceValueOf 1)
          ]
  void (submitTxConstraints (OnChain.νHydraInstance policyId) constraints)

collectCom ::
  (AsContractError e) =>
  Contract () Schema e ()
collectCom =
  endpoint @"collectCom" @()

-- FIXME

type Schema =
  BlockchainActions
    .\/ Endpoint "init" [PubKeyHash]
    .\/ Endpoint "collectCom" ()

contract ::
  (AsContractError e) =>
  MonetaryPolicyHash ->
  Contract () Schema e ()
contract policyId = endpoints
 where
  endpoints = init policyId `select` collectCom
