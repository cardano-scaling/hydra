{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.OffChain where

import Hydra.Prelude hiding (init)

import Ledger

import Hydra.Contract.OnChain as OnChain (asDatum, asRedeemer)
import Ledger.Ada (lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints.OffChain (ScriptLookups (..))
import Ledger.Constraints.TxConstraints (
  TxConstraints,
  mustBeSignedBy,
  mustIncludeDatum,
  mustMintCurrency,
  mustMintValue,
  mustPayToOtherScript,
  mustPayToPubKey,
  mustPayToTheScript,
  mustSpendPubKeyOutput,
  mustSpendScriptOutput,
 )
import Ledger.Credential (Credential (..))
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (..))
import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  awaitTxConfirmed,
  endpoint,
  logInfo,
  select,
  submitTxConstraints,
  submitTxConstraintsWith,
  tell,
  utxoAt,
  type (.\/),
 )

import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OnChain as OnChain
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value

--
-- Head Parameters
--

data HeadParameters = HeadParameters
  { participants :: [PubKeyHash]
  , policy :: MintingPolicy
  , policyId :: MintingPolicyHash
  }

mkHeadParameters :: [PubKeyHash] -> MintingPolicy -> HeadParameters
mkHeadParameters participants policy =
  HeadParameters{participants, policy, policyId}
 where
  policyId = mintingPolicyHash policy

--
-- Init
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
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
init params@HeadParameters{participants, policy, policyId} = do
  parties <- endpoint @"init" @()
  logInfo $ "init: should post tx announcing these parties: " <> show @Text parties
  void $ submitTxConstraintsWith lookups constraints
  tell [OnChain.Initial]
 where
  lookups =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slTypedValidator =
          Just (hydraTypedValidator params)
      }

  constraints =
    let datumInitial = OnChain.mkDatumInitial (toOnChainHeadParameters params)
     in mconcat
          [ foldMap
              ( \vk ->
                  let participationToken = OnChain.mkParty policyId vk
                   in mconcat
                        [ mustPayToOtherScript
                            (Scripts.validatorHash $ initialTypedValidator params)
                            (asDatum @(DatumType OnChain.Initial) (datumInitial vk))
                            participationToken
                        , mustMintValue
                            participationToken
                        ]
              )
              participants
          , mustPayToTheScript OnChain.Initial (lovelaceValueOf 0)
          ]

-- To lock outputs for a Hydra head, the i-th head member will attach a commit
-- transaction to the i-th output of the initial transaction.
--
-- Validator `νCommit` ensures that the commit transaction correctly records the
-- partial UTxO set Ui committed by the party.
commit ::
  (AsContractError e) =>
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
commit params@HeadParameters{policy, policyId} = do
  (vk, toCommit) <- endpoint @"commit" @(PubKeyHash, (TxOutRef, TxOutTx))
  initial <-
    utxoAtWithDatum
      (Scripts.validatorAddress $ initialTypedValidator params)
      (asDatum @(DatumType OnChain.Initial) (datumInitial vk))
  void $
    submitTxConstraintsWith
      (lookups vk toCommit initial)
      (constraints vk toCommit initial)
 where
  datumInitial = OnChain.mkDatumInitial (toOnChainHeadParameters params)

  lookups vk (ref, txOut) initial =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slTypedValidator =
          Just (hydraTypedValidator params)
      , slTxOutputs =
          initial <> Map.singleton ref txOut
      , slOtherScripts =
          Map.fromList
            [ (Scripts.validatorAddress script, Scripts.validatorScript script)
            | SomeTypedValidator script <-
                [ SomeTypedValidator $ initialTypedValidator params
                ]
            ]
      , slOwnPubkey =
          Just vk
      }

  constraints vk (ref, txOut) initial =
    let amount = txOutValue (txOutTxOut txOut) <> OnChain.mkParty policyId vk
     in mconcat
          [ mustBeSignedBy vk
          , -- NOTE: using a 'foldMap' here but that 'initial' utxo really has only one
            -- element!
            foldMap
              (`mustSpendScriptOutput` asRedeemer @(RedeemerType OnChain.Initial) ref)
              (Map.keys initial)
          , mustSpendPubKeyOutput ref
          , mustPayToOtherScript
              (Scripts.validatorHash $ commitTypedValidator params)
              (asDatum @(DatumType OnChain.Commit) (txOutTxOut txOut))
              amount
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
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
collectCom params@HeadParameters{participants, policy, policyId} = do
  (headMember, storedOutputs) <- endpoint @"collectCom" @(PubKeyHash, [TxOut])
  commits <- utxoAt (Scripts.validatorAddress $ commitTypedValidator params)
  stateMachine <- utxoAt (Scripts.validatorAddress $ hydraTypedValidator params)
  tx <-
    submitTxConstraintsWith @OnChain.Hydra
      (lookups commits stateMachine headMember)
      (constraints commits stateMachine headMember storedOutputs)
  awaitTxConfirmed (txId tx)
  tell [OnChain.Open storedOutputs]
 where
  lookups commits stateMachine headMember =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slTypedValidator =
          Just (hydraTypedValidator params)
      , slOtherScripts =
          Map.fromList
            [ (Scripts.validatorAddress script, Scripts.validatorScript script)
            | SomeTypedValidator script <-
                [ SomeTypedValidator $ commitTypedValidator params
                , SomeTypedValidator $ hydraTypedValidator params
                ]
            ]
      , slTxOutputs =
          commits <> stateMachine
      , slOwnPubkey =
          Just headMember
      }

  constraints commits stateMachine headMember storedOutputs =
    let amount = foldMap (txOutValue . snd) $ flattenUtxo (commits <> stateMachine)
     in mconcat
          [ mustBeSignedBy headMember
          , foldMap
              (\ref -> mustSpendScriptOutput ref $ asRedeemer @(RedeemerType OnChain.Hydra) OnChain.CollectCom)
              (Map.keys stateMachine)
          , foldMap
              (\(_, ref) -> mustSpendScriptOutput ref $ asRedeemer @(RedeemerType OnChain.Commit) ())
              (zipOnParty policyId participants commits)
          , foldMap
              (mustIncludeDatum . asDatum @(DatumType OnChain.Commit))
              storedOutputs
          , mustPayToTheScript (OnChain.Open $ reverse storedOutputs) amount
          ]

abort ::
  (AsContractError e) =>
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
abort params@HeadParameters{participants, policy, policyId} = do
  (headMember, toRefund) <- endpoint @"abort" @(PubKeyHash, [TxOut])
  initial <- utxoAt (Scripts.validatorAddress $ initialTypedValidator params)
  commits <- utxoAt (Scripts.validatorAddress $ commitTypedValidator params)
  stateMachine <- utxoAt (Scripts.validatorAddress $ hydraTypedValidator params)
  tx <-
    submitTxConstraintsWith @OnChain.Hydra
      (lookups initial commits stateMachine headMember)
      (constraints initial commits stateMachine toRefund headMember)
  awaitTxConfirmed (txId tx)
  tell [OnChain.Final]
 where
  lookups initial commits stateMachine headMember =
    mempty
      { slMPS =
          Map.singleton policyId policy
      , slTypedValidator =
          Just (hydraTypedValidator params)
      , slOtherScripts =
          Map.fromList
            [ (Scripts.validatorAddress script, Scripts.validatorScript script)
            | SomeTypedValidator script <-
                [ SomeTypedValidator $ initialTypedValidator params
                , SomeTypedValidator $ commitTypedValidator params
                , SomeTypedValidator $ hydraTypedValidator params
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
          (\vk -> mustMintCurrency policyId (OnChain.mkPartyName vk) (-1))
          participants
      , foldMap mustRefund toRefund
      , foldMap
          (\(_vk, ref) -> mustSpendScriptOutput ref $ asRedeemer @(RedeemerType OnChain.Initial) ref)
          (zipOnParty policyId participants initial)
      , foldMap
          (\(_vk, ref) -> mustSpendScriptOutput ref $ asRedeemer @(RedeemerType OnChain.Commit) ())
          (zipOnParty policyId participants commits)
      , foldMap
          (`mustSpendScriptOutput` asRedeemer @(RedeemerType OnChain.Hydra) OnChain.Abort)
          (Map.keys stateMachine)
      ]

-- | This endpoint allows for setting up a wallet for testing, that is, a wallet
-- that has several UTxO, so that one can be locked and the other used to pay
-- for fees.
setupForTesting ::
  (AsContractError e) =>
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
setupForTesting params = do
  (vk, coins) <- endpoint @"setupForTesting" @(PubKeyHash, [Value])
  tx <- submitTxConstraints (hydraTypedValidator params) (constraints vk coins)
  awaitTxConfirmed (txId tx)
 where
  constraints vk =
    foldMap (mustPayToPubKey vk)

type Schema =
  Endpoint "setupForTesting" (PubKeyHash, [Value])
    .\/ Endpoint "init" ()
    .\/ Endpoint "commit" (PubKeyHash, (TxOutRef, TxOutTx))
    .\/ Endpoint "collectCom" (PubKeyHash, [TxOut])
    .\/ Endpoint "abort" (PubKeyHash, [TxOut])

contract ::
  (AsContractError e) =>
  HeadParameters ->
  Contract [OnChain.State] Schema e ()
contract params = forever endpoints
 where
  endpoints =
    setupForTesting params
      `select` init params
      `select` commit params
      `select` collectCom params
      `select` abort params

--
-- Helpers
--

data SomeTypedValidator = forall a. SomeTypedValidator (TypedValidator a)

hydraTypedValidator :: HeadParameters -> Scripts.TypedValidator OnChain.Hydra
hydraTypedValidator =
  OnChain.hydraTypedValidator . toOnChainHeadParameters

initialTypedValidator :: HeadParameters -> Scripts.TypedValidator OnChain.Initial
initialTypedValidator =
  OnChain.initialTypedValidator . toOnChainHeadParameters

commitTypedValidator :: HeadParameters -> Scripts.TypedValidator OnChain.Commit
commitTypedValidator =
  OnChain.commitTypedValidator . toOnChainHeadParameters

toOnChainHeadParameters :: HeadParameters -> OnChain.HeadParameters
toOnChainHeadParameters HeadParameters{participants, policyId} =
  OnChain.HeadParameters participants policyId

flattenUtxo :: UtxoMap -> [(TxOutRef, TxOut)]
flattenUtxo =
  fmap (second txOutTxOut) . Map.assocs

mustRefund ::
  forall i o.
  TxOut ->
  TxConstraints i o
mustRefund txOut =
  case txOutAddress txOut of
    (Address (PubKeyCredential vk) _) ->
      mustPayToPubKey vk (txOutValue txOut)
    (Address ScriptCredential{} _) ->
      error "mustRefund: committing script output isn't allowed."

utxoAtWithDatum ::
  forall w s e.
  (AsContractError e) =>
  Address ->
  Datum ->
  Contract w s e UtxoMap
utxoAtWithDatum addr datum = do
  utxo <- utxoAt addr
  pure $ Map.filter matchDatum utxo
 where
  matchDatum txOut =
    txOutDatumHash (txOutTxOut txOut) == Just (datumHash datum)

-- When collecting commits, we need to associate each commit utxo to the
-- corresponding head member keys. There's no guarantee that head members will
-- commit in the order they are defined in the head parameters, so just
-- "zipping" doesn't do it.
--
-- Instead, we must associate each commited utxo to their key using the
-- participation token that they all carry.
zipOnParty ::
  MintingPolicyHash ->
  [PubKeyHash] ->
  UtxoMap ->
  [(PubKeyHash, TxOutRef)]
zipOnParty policyId vks utxo =
  go [] (flattenUtxo utxo) vks []
 where
  go acc [] _ _ = acc
  go acc _ [] _ = acc
  go acc (u : qu) (vk : qv) qv' =
    if u `hasParty` vk
      then go ((vk, fst u) : acc) qu (qv ++ qv') []
      else go acc (u : qu) qv (vk : qv')

  hasParty :: (TxOutRef, TxOut) -> PubKeyHash -> Bool
  hasParty (_, txOut) vk =
    let currency = Value.mpsSymbol policyId
        token = OnChain.mkPartyName vk
     in Value.valueOf (txOutValue txOut) currency token > 0
