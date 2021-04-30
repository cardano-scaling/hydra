{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OffChain where

import Prelude hiding (init)

import Control.Arrow (second)
import Control.Monad (forever, void)
import Hydra.Contract.OnChain (HeadParameters (..))
import Ledger hiding (out, value)
import Ledger.Ada (lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints.OffChain (ScriptLookups (..))
import Ledger.Constraints.TxConstraints (
  TxConstraints,
  mustBeSignedBy,
  mustForgeCurrency,
  mustForgeValue,
  mustPayToOtherScript,
  mustPayToPubKey,
  mustPayToTheScript,
  mustSpendPubKeyOutput,
  mustSpendScriptOutput,
 )
import Ledger.Credential (Credential (..))
import Ledger.Typed.Scripts (ScriptInstance)
import Plutus.Contract (
  AsContractError,
  BlockchainActions,
  Contract,
  Endpoint,
  endpoint,
  logInfo,
  select,
  submitTxConstraints,
  submitTxConstraintsWith,
  tell,
  utxoAt,
  type (.\/),
 )
import Plutus.Contract.Effects.AwaitTxConfirmed (awaitTxConfirmed)
import Plutus.Contract.Effects.UtxoAt (HasUtxoAt)

import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OnChain as OnChain
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value

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
  [PubKeyHash] ->
  MonetaryPolicy ->
  Contract [OnChain.HydraState] Schema e ()
init vks policy = do
  endpoint @"init" @()
  void $ submitTxConstraintsWith lookups constraints
  tell [OnChain.Initial vks]
 where
  policyId =
    monetaryPolicyHash policy

  headParameters =
    HeadParameters vks policyId

  lookups =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.hydra headParameters)
      }

  constraints =
    mconcat
      [ foldMap
          ( \vk ->
              let participationToken = OnChain.mkParticipationToken policyId vk
               in mconcat
                    [ mustPayToOtherScript
                        (OnChain.initialHash headParameters)
                        (OnChain.asDatum vk)
                        participationToken
                    , mustForgeValue
                        participationToken
                    ]
          )
          vks
      , mustPayToTheScript (OnChain.Initial vks) (lovelaceValueOf 0)
      ]

-- To lock outputs for a Hydra head, the i-th head member will attach a commit
-- transaction to the i-th output of the initial transaction.
--
-- Validator `νCommit` ensures that the commit transaction correctly records the
-- partial UTxO set Ui committed by the party.
commit ::
  (AsContractError e) =>
  [PubKeyHash] ->
  MonetaryPolicy ->
  Contract [OnChain.HydraState] Schema e ()
commit vks policy = do
  (vk, toCommit) <- endpoint @"commit" @(PubKeyHash, (TxOutRef, TxOutTx))
  initial <- utxoAtWithDatum (OnChain.initialAddress headParameters) (OnChain.asDatum vk)
  void $ submitTxConstraintsWith (lookups vk toCommit initial) (constraints vk toCommit initial)
 where
  policyId =
    monetaryPolicyHash policy

  headParameters =
    HeadParameters vks policyId

  lookups vk (ref, out) initial =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.hydra headParameters)
      , slTxOutputs =
          initial <> Map.singleton ref out
      , slOtherScripts =
          let script = OnChain.initial headParameters
           in Map.fromList
                [ (Scripts.scriptAddress script, Scripts.validatorScript script)
                ]
      , slOwnPubkey =
          Just vk
      }

  constraints vk (ref, out) initial =
    let value = txOutValue (txOutTxOut out) <> OnChain.mkParticipationToken policyId vk
     in mconcat
          [ mustBeSignedBy vk
          , -- NOTE: using a 'foldMap' here but that 'initial' utxo really has only one
            -- element!
            foldMap (`mustSpendScriptOutput` OnChain.initialRedeemer ref) (Map.keys initial)
          , mustSpendPubKeyOutput ref
          , mustPayToOtherScript
              (OnChain.commitHash headParameters)
              (OnChain.commitDatum $ txOutTxOut out)
              value
          ]

-- The SM transition from initial to open is achieved by posting the collectCom
-- transaction.
--
-- All head parameters remain part of the state.
--
-- In addition, information about the initial UTxO set, which is made up of the
-- individual UTxO sets Ui collected from the commit transactions, is stored in
-- the state.
--
-- It is also required that all n participation tokens be present in the output
-- of the collectCom transaction.
--
-- Finally, note that the transition requires a proof that the signer is one of
-- the head members.
collectCom ::
  (AsContractError e) =>
  [PubKeyHash] ->
  MonetaryPolicy ->
  Contract [OnChain.HydraState] Schema e ()
collectCom vks policy = do
  (headMember, storedOutputs) <- endpoint @"collectCom" @(PubKeyHash, [TxOut])
  commits <- utxoAt (OnChain.commitAddress headParameters)
  logInfo @String $ "commits: " <> show (length commits)
  stateMachine <- utxoAt (OnChain.hydraAddress headParameters)
  tx <-
    submitTxConstraintsWith @OnChain.Hydra
      (lookups commits stateMachine headMember storedOutputs)
      (constraints commits stateMachine headMember storedOutputs)
  awaitTxConfirmed (txId tx)
  tell [OnChain.Open storedOutputs]
 where
  policyId =
    monetaryPolicyHash policy

  headParameters =
    HeadParameters vks policyId

  lookups commits stateMachine headMember storedOutputs =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.hydra headParameters)
      , slOtherScripts =
          Map.fromList
            [ (Scripts.scriptAddress script, Scripts.validatorScript script)
            | SomeScriptInstance script <-
                [ SomeScriptInstance $ OnChain.commit headParameters
                , SomeScriptInstance $ OnChain.hydra headParameters
                ]
            ]
      , slTxOutputs =
          commits <> stateMachine
      , slOwnPubkey =
          Just headMember
      , slOtherData =
          Map.fromList [let d = OnChain.asDatum out in (datumHash d, d) | out <- storedOutputs]
      }

  constraints commits stateMachine headMember storedOutputs =
    let value = foldMap (txOutValue . snd) $ flattenUtxo (commits <> stateMachine)
     in mconcat
          [ mustBeSignedBy headMember
          , mustPayToTheScript (OnChain.Open storedOutputs) value
          , foldMap
              (\(_vk, ref) -> mustSpendScriptOutput ref $ OnChain.asRedeemer ())
              (zipOnParticipationToken policyId vks commits)
          , -- NOTE: using a 'foldMap' here but the 'stateMachine' utxo really
            -- has only one element!
            foldMap
              (`mustSpendScriptOutput` OnChain.asRedeemer OnChain.CollectCom)
              (Map.keys stateMachine)
          ]

abort ::
  (AsContractError e) =>
  [PubKeyHash] ->
  MonetaryPolicy ->
  Contract [OnChain.HydraState] Schema e ()
abort vks policy = do
  (headMember, toRefund) <- endpoint @"abort" @(PubKeyHash, [TxOut])
  initial <- utxoAt (OnChain.initialAddress headParameters)
  commits <- utxoAt (OnChain.commitAddress headParameters)
  stateMachine <- utxoAt (OnChain.hydraAddress headParameters)
  tx <-
    submitTxConstraintsWith @OnChain.Hydra
      (lookups initial commits stateMachine headMember)
      (constraints initial commits stateMachine toRefund headMember)
  awaitTxConfirmed (txId tx)
  tell [OnChain.Final]
 where
  policyId =
    monetaryPolicyHash policy

  headParameters =
    HeadParameters vks policyId

  lookups initial commits stateMachine headMember =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slScriptInstance =
          Just (OnChain.hydra headParameters)
      , slOtherScripts =
          Map.fromList
            [ (Scripts.scriptAddress script, Scripts.validatorScript script)
            | SomeScriptInstance script <-
                [ SomeScriptInstance $ OnChain.initial headParameters
                , SomeScriptInstance $ OnChain.commit headParameters
                , SomeScriptInstance $ OnChain.hydra headParameters
                ]
            ]
      , slTxOutputs =
          initial <> commits <> stateMachine
      , slOwnPubkey =
          Just headMember
      }

  constraints initial commits stateMachine toRefund headMember =
    mconcat
      [ mustBeSignedBy headMember
      , mustPayToTheScript OnChain.Final (lovelaceValueOf 0)
      , foldMap
          (\vk -> mustForgeCurrency policyId (OnChain.mkParticipationTokenName vk) (-1))
          vks
      , foldMap mustRefund toRefund
      , foldMap
          (\(_vk, ref) -> mustSpendScriptOutput ref (OnChain.initialRedeemer ref))
          (zipOnParticipationToken policyId vks initial)
      , foldMap
          (\(_vk, ref) -> mustSpendScriptOutput ref OnChain.commitRedeemer)
          (zipOnParticipationToken policyId vks commits)
      , foldMap
          (`mustSpendScriptOutput` OnChain.asRedeemer OnChain.Abort)
          (Map.keys stateMachine)
      ]

-- | This endpoint allows for setting up a wallet for testing, that is, a wallet
-- that has several UTxO, so that one can be locked and the other used to pay
-- for fees.
setupForTesting ::
  (AsContractError e) =>
  [PubKeyHash] ->
  MonetaryPolicy ->
  Contract [OnChain.HydraState] Schema e ()
setupForTesting vks policy = do
  (vk, coins) <- endpoint @"setupForTesting" @(PubKeyHash, [Value])
  tx <- submitTxConstraints (OnChain.hydra headParameters) (constraints vk coins)
  awaitTxConfirmed (txId tx)
 where
  policyId =
    monetaryPolicyHash policy

  headParameters =
    HeadParameters vks policyId

  constraints vk =
    foldMap (mustPayToPubKey vk)

type Schema =
  BlockchainActions
    .\/ Endpoint "setupForTesting" (PubKeyHash, [Value])
    .\/ Endpoint "init" ()
    .\/ Endpoint "commit" (PubKeyHash, (TxOutRef, TxOutTx))
    .\/ Endpoint "collectCom" (PubKeyHash, [TxOut])
    .\/ Endpoint "abort" (PubKeyHash, [TxOut])

contract ::
  (AsContractError e) =>
  MonetaryPolicy ->
  [PubKeyHash] ->
  Contract [OnChain.HydraState] Schema e ()
contract policy vks = forever endpoints
 where
  endpoints =
    setupForTesting vks policy
      `select` init vks policy
      `select` commit vks policy
      `select` collectCom vks policy
      `select` abort vks policy

--
-- Helpers
--

data SomeScriptInstance = forall a. SomeScriptInstance (ScriptInstance a)

flattenUtxo :: UtxoMap -> [(TxOutRef, TxOut)]
flattenUtxo =
  fmap (second txOutTxOut) . Map.assocs

mustRefund ::
  forall i o.
  TxOut ->
  TxConstraints i o
mustRefund out =
  case txOutAddress out of
    (Address (PubKeyCredential vk) _) ->
      mustPayToPubKey vk (txOutValue out)
    (Address ScriptCredential{} _) ->
      error "mustRefund: committing script output isn't allowed."

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
    txOutDatumHash (txOutTxOut out) == Just (datumHash datum)

-- When collecting commits, we need to associate each commit utxo to the
-- corresponding head member keys. There's no guarantee that head members will
-- commit in the order they are defined in the head parameters, so just
-- "zipping" doesn't do it.
--
-- Instead, we must associate each commited utxo to their key using the
-- participation token that they all carry.
zipOnParticipationToken ::
  MonetaryPolicyHash ->
  [PubKeyHash] ->
  UtxoMap ->
  [(PubKeyHash, TxOutRef)]
zipOnParticipationToken policyId vks utxo =
  go [] (flattenUtxo utxo) vks []
 where
  go acc [] _ _ = acc
  go acc _ [] _ = acc
  go acc (u : qu) (vk : qv) qv' =
    if u `hasParticipationToken` vk
      then go ((vk, fst u) : acc) qu (qv ++ qv') []
      else go acc (u : qu) qv (vk : qv')

  hasParticipationToken :: (TxOutRef, TxOut) -> PubKeyHash -> Bool
  hasParticipationToken (_, out) vk =
    let currency = Value.mpsSymbol policyId
        token = OnChain.mkParticipationTokenName vk
     in Value.valueOf (txOutValue out) currency token > 0
