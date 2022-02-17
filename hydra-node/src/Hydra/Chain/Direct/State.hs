{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.State (
  -- * OnChainHeadState'
  OnChainHeadState',
  HeadStateKind (..),

  -- ** Common
  commitAddress,

  -- ** Idle
  idleOnChainHeadState',

  -- ** Initialized
  initialOutputRef,
  mkCommitOutput,
  findInitialRedeemer,
  putCommit,

  -- ** Open

  -- ** Closed
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Binary (serialize')
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import Hydra.Contract.Initial (Initial)
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.Party (partyFromVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Party (Party, vkey)
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.V1.Ledger.Api (
  Datum,
  MintingPolicyHash,
  ValidatorHash,
  fromData,
  toBuiltin,
 )
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)

-- | An opaque on-chain head state, which records information and events
-- happening on the layer-1 for a given Hydra head.
data OnChainHeadState' (st :: HeadStateKind) = OnChainHeadState'
  { networkId :: NetworkId
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , stateMachine :: HydraStateMachine st
  }

-- NOTE (1): The state machine UTxO produced by the Init transaction (a.k.a
-- 'threadOutput') is always present and 'threaded' across all transactions.
--
-- NOTE (2): The Head's identifier is somewhat encoded in the TxOut's address
--
-- TODO: Data and [OnChain.Party] are overlapping
--
-- TODO: There are better ways to model this to avoid duplicating common fields
-- across all branches!
data HydraStateMachine (st :: HeadStateKind) where
  Idle :: HydraStateMachine 'StIdle
  Initial ::
    { initialThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    , initialInitials :: [UTxOWithScript]
    , initialCommits :: [UTxOWithScript]
    } ->
    HydraStateMachine 'StInitialized
  Open ::
    { openThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    } ->
    HydraStateMachine 'StOpen
  Closed ::
    { closedThreadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
    } ->
    HydraStateMachine 'StClosed

-- | Some Kind for witnessing Hydra state-machine's states at the type-level.
--
-- This is useful to
--
-- (a) Reads code evolving the state machine, as it makes transition more
-- obvious from type-signatures;
-- (b) Pattern-match on the 'HydraStateMachine' without having to bother with
-- non-reachable cases.
data HeadStateKind = StIdle | StInitialized | StOpen | StClosed

-- Common

commitAddress :: OnChainHeadState' st -> AddressInEra
commitAddress OnChainHeadState'{networkId} =
  mkScriptAddress @PlutusScriptV1 networkId commitScript
 where
  commitScript = fromPlutusScript Commit.validatorScript

-- Idle

-- | Initialize a new 'OnChainHeadState''.
idleOnChainHeadState' ::
  NetworkId ->
  VerificationKey PaymentKey ->
  Party ->
  OnChainHeadState' 'StIdle
idleOnChainHeadState' networkId ownVerificationKey ownParty =
  OnChainHeadState'
    { networkId
    , ownVerificationKey
    , ownParty
    , stateMachine = Idle
    }

-- Initialized

-- | Retrieve the initial output reference (i.e. locked by an initial script)
-- and a corresponding spending witness.
initialOutputRef ::
  Maybe (TxIn, TxOut CtxUTxO) ->
  OnChainHeadState' 'StInitialized ->
  Maybe (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn))
initialOutputRef utxo OnChainHeadState'{ownVerificationKey, stateMachine} = do
  (outRef, vkh) <- ownInitial ownVerificationKey initialInitials
  let initialScript =
        fromPlutusScript @PlutusScriptV1 Initial.validatorScript
  let initialDatum =
        mkScriptDatum $ Initial.datum $ toPlutusKeyHash vkh
  let initialRedeemer =
        toScriptData . Initial.redeemer $
          Initial.Commit (toPlutusTxOutRef . fst <$> utxo)
  return
    ( outRef
    , BuildTxWith $ mkScriptWitness initialScript initialDatum initialRedeemer
    )
 where
  Initial{initialInitials} = stateMachine

mkCommitOutput ::
  Maybe (TxIn, TxOut CtxUTxO) ->
  OnChainHeadState' 'StInitialized ->
  TxOut CtxTx
mkCommitOutput mUTxO st@OnChainHeadState'{ownParty} =
  TxOut (commitAddress st) commitValue commitDatum
 where
  -- FIXME: We should add the value from the initialIn too because it contains the PTs
  commitValue =
    headValue <> maybe mempty (txOutValue . snd) mUTxO
  commitDatum =
    mkTxOutDatum $ mkCommitDatum ownParty (Head.validatorHash policyId) mUTxO

findInitialRedeemer ::
  Tx ->
  OnChainHeadState' 'StInitialized ->
  Maybe (RedeemerType Initial)
findInitialRedeemer tx OnChainHeadState'{stateMachine} = do
  findInitialOutputRef >>= findRedeemerSpending tx
 where
  Initial{initialInitials} = stateMachine

  findInitialOutputRef =
    case filter (`elem` (fst3 <$> initialInitials)) (txIns' tx) of
      [input] -> Just input
      _ -> Nothing

putCommit ::
  UTxOWithScript ->
  OnChainHeadState' 'StInitialized ->
  OnChainHeadState' 'StInitialized
putCommit commit st@OnChainHeadState'{stateMachine} =
  st
    { stateMachine =
        stateMachine
          { initialCommits = commit : initialCommits
          }
    }
 where
  Initial{initialCommits} = stateMachine

--
-- Helpers
--

type UTxOWithScript = (TxIn, TxOut CtxUTxO, ScriptData)

fst3 :: (a, b, c) -> a
fst3 (a, _b, _c) = a

-- FIXME: should not be hardcoded but come from the previous state!
headValue :: Value
headValue = lovelaceToValue (Lovelace 2_000_000)

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "token")

-- FIXME: This should be part of the OnChainHeadState
policyId :: MintingPolicyHash
(policyId, _) = first currencyMPSHash (unAssetClass threadToken)

mkCommitDatum :: Party -> ValidatorHash -> Maybe (TxIn, TxOut CtxUTxO) -> Datum
mkCommitDatum (partyFromVerKey . vkey -> party) headValidatorHash utxo =
  Commit.datum (party, headValidatorHash, serializedUTxO)
 where
  serializedUTxO = case utxo of
    Nothing ->
      Nothing
    Just (_i, o) ->
      Just $ Commit.SerializedTxOut (toBuiltin $ serialize' $ toLedgerTxOut o)

-- | Look for the "initial" which corresponds to given cardano verification key.
ownInitial ::
  VerificationKey PaymentKey ->
  [UTxOWithScript] ->
  Maybe (TxIn, Hash PaymentKey)
ownInitial vkey =
  foldl' go Nothing
 where
  go (Just x) _ = Just x
  go Nothing (i, _, dat) = do
    let vkh = verificationKeyHash vkey
    pkh <- fromData (toPlutusData dat)
    guard $ pkh == toPlutusKeyHash vkh
    pure (i, vkh)
