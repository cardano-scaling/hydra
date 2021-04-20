{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OffChain where

import PlutusTx.Prelude hiding (init)

import Control.Arrow (second)
import Control.Monad (forever, void)
import Ledger
import Ledger.Ada (lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints.OffChain (ScriptLookups (..))
import Plutus.Contract.Effects.UtxoAt (HasUtxoAt)

import Plutus.Contract (
  AsContractError,
  BlockchainActions,
  Contract,
  Endpoint,
  endpoint,
  logInfo,
  select,
  submitTxConstraintsWith,
  utxoAt,
  type (.\/),
 )

import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OnChain as OnChain
import qualified Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Prelude

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
  MonetaryPolicy ->
  Contract () Schema e ()
init policy = do
  vks <- endpoint @"init" @[PubKeyHash]
  void $ submitTxConstraintsWith lookups (constraints vks)
 where
  policyId =
    monetaryPolicyHash policy

  lookups =
    Prelude.mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.νHydraInstance policyId)
      }

  constraints vks =
    mconcat
      [ foldMap
          ( \vk ->
              let participationToken = OnChain.mkParticipationToken policyId vk
               in mconcat
                    [ Constraints.mustPayToOtherScript
                        (OnChain.νInitialHash policyId)
                        (OnChain.δInitial vk)
                        participationToken
                    , Constraints.mustForgeValue
                        participationToken
                    ]
          )
          vks
      , Constraints.mustPayToTheScript state (lovelaceValueOf 1)
      ]
   where
    state = OnChain.Initial

-- To lock outputs for a Hydra head, the i-th head member will attach a commit
-- transaction to the i-th output of the initial transaction.
--
-- Validator `νCommit` ensures that the commit transaction correctly records the
-- partial UTxO set Ui committed by the party.
commit ::
  (AsContractError e) =>
  MonetaryPolicy ->
  Contract () Schema e ()
commit policy = do
  (vk, toCommit) <- endpoint @"commit" @(PubKeyHash, UtxoMap)
  utxo <- utxoAtWithDatum (OnChain.νInitialAddress policyId) (OnChain.δInitial vk)
  logInfo @String $ show utxo
  void $
    submitTxConstraintsWith
      (lookups vk toCommit utxo)
      (constraints vk toCommit utxo)
 where
  policyId =
    monetaryPolicyHash policy

  lookups vk toCommit utxo =
    Prelude.mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.νInitialInstance policyId)
      , slTxOutputs =
          utxo Prelude.<> toCommit
      , slOtherScripts =
          let scriptA = OnChain.νCommitInstance policyId
              scriptB = OnChain.νInitialInstance policyId
           in Map.fromList
                [ (Scripts.scriptAddress scriptA, Scripts.validatorScript scriptA)
                , (Scripts.scriptAddress scriptB, Scripts.validatorScript scriptB)
                ]
      , slOtherData =
          let datum = OnChain.δCommit (Map.keys toCommit)
           in Map.fromList
                [(datumHash datum, datum)]
      , slOwnPubkey =
          Just vk
      }

  constraints vk toCommit utxo =
    mconcat
      [ OnChain.mustBeSignedBy vk
      , OnChain.mustCommitUtxos (OnChain.νCommitHash policyId) (flattenUtxo toCommit)
      , OnChain.mustForwardParticipationToken policyId vk
      , foldMap (`Constraints.mustSpendScriptOutput` OnChain.ρInitial) (Map.keys utxo)
      ]

collectCom ::
  (AsContractError e) =>
  Contract () Schema e ()
collectCom =
  endpoint @"collectCom" @()

-- FIXME

type Schema =
  BlockchainActions
    .\/ Endpoint "init" [PubKeyHash]
    .\/ Endpoint "commit" (PubKeyHash, UtxoMap)
    .\/ Endpoint "collectCom" ()

contract ::
  (AsContractError e) =>
  MonetaryPolicy ->
  Contract () Schema e ()
contract policy = forever endpoints
 where
  endpoints =
    init policy
      `select` commit policy
      `select` collectCom

--
-- Helpers
--

flattenUtxo :: UtxoMap -> [(TxOutRef, Value)]
flattenUtxo =
  fmap (second (txOutValue . txOutTxOut)) . Map.assocs

utxoAtWithDatum ::
  forall w s e.
  (AsContractError e, HasUtxoAt s) =>
  Address ->
  Datum ->
  Contract w s e UtxoMap
utxoAtWithDatum addr datum = do
  utxo <- utxoAt addr
  pure $ Map.filter matchDatum utxo
 where
  matchDatum out =
    case txOutType (txOutTxOut out) of
      PayToScript d | d == datumHash datum -> True
      _ -> False
