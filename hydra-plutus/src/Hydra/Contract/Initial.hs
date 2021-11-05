{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of participation tokens.
module Hydra.Contract.Initial where

import Control.Monad (forM_, void)
import qualified Data.Map as M
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import Hydra.OnChain.Util (findUtxo, mkParty, mustRunContract)
import Ledger hiding (validatorHash, unspentOutputs)
import qualified Ledger.Ada as Ada
import Ledger.Constraints (
  checkScriptContext,
  mintingPolicy,
  otherScript,
  typedValidatorLookups,
  unspentOutputs
 )
import Ledger.Constraints.TxConstraints (
  TxConstraints,
  mustBeSignedBy,
  mustMintValue,
  mustPayToOtherScript,
  mustPayToTheScript,
  mustPayToPubKey,
  mustSpendPubKeyOutput,
  mustSpendScriptOutput
 )
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import qualified Plutus.Trace.Emulator as Trace
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import PlutusTx.IsData.Class (ToData (..))
import Prelude (Int, Semigroup (..))
import Wallet.Emulator (knownWallet)

data Initial

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = (MintingPolicyHash, Dependencies, PubKeyHash)
  type RedeemerType Initial = Maybe TxOutRef

-- TODO: We should be able to get rid of this in principle and inject them
-- directly at compile-time (since they are statically known). Somehow, this
-- doesn't work out of the box with the Plutus plugin at the moment and we
-- resort to inject them 'manually'.
data Dependencies = Dependencies
  { headScript :: ValidatorHash
  , commitScript :: ValidatorHash
  }

PlutusTx.makeLift ''Dependencies
PlutusTx.unstableMakeIsData ''Dependencies

validator ::
  (MintingPolicyHash, Dependencies, PubKeyHash) ->
  Maybe TxOutRef ->
  ScriptContext ->
  Bool
validator (policyId, Dependencies{headScript, commitScript}, vk) mref ctx =
  -- if-then-else is cheaper!
  if consumedByCommit then True else consumedByAbort

  -- || is more expensive!
  -- consumedByCommit || consumedByAbort
 where
  -- A commit transaction, identified by:
  --    (a) A signature that verifies as valid with verification key defined as datum
  --    (b) Spending a UTxO also referenced as redeemer.
  --    (c) Having the commit validator in its only output, with a valid
  --        participation token for the associated key, and the total value of the
  --        committed UTxO.
  consumedByCommit =
    case mref >>= findUtxo ctx of
      Nothing ->
        False
      Just utxo ->
        let commitDatum = Commit.datum (Commit.Dependencies headScript, snd utxo)
            commitValue = txOutValue (snd utxo) <> mkParty policyId vk
         in checkScriptContext @(RedeemerType Initial) @(DatumType Initial)
              ( mconcat
                  [ mustBeSignedBy vk
                  , mustSpendPubKeyOutput (fst utxo)
                  , mustPayToOtherScript commitScript commitDatum commitValue
                  ]
              )
              ctx

  consumedByAbort =
    mustRunContract @(RedeemerType Head.Head) headScript Head.Abort ctx

compiledValidator :: CompiledCode (ValidatorType Initial)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Initial
typedValidator = Scripts.mkTypedValidator @Initial
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)
{- ORMOLU_ENABLE -}

-- | Do not use this outside of plutus land.
validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Initial -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType Initial -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

-- | Do not use this outside of plutus land.
address :: Address
address = scriptHashAddress validatorHash

mustPayToScript ::
  forall i o.
  MintingPolicyHash ->
  Dependencies ->
  PubKeyHash ->
  Value ->
  TxConstraints i o
mustPayToScript policyId dependencies pubKey =
  mustPayToOtherScript validatorHash $ datum (policyId, dependencies, pubKey)

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

-- FIXME: Drunk code for the lazy-condition experiment only
-- A always-true fake minting policy for testing
{-# INLINEABLE fakeMintingValidator #-}
fakeMintingValidator :: () -> ScriptContext -> Bool
fakeMintingValidator _ _ = True

fakeMintingPolicy :: MintingPolicy
fakeMintingPolicy =
  mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy fakeMintingValidator||])

fakeMintingPolicyHash :: MintingPolicyHash
fakeMintingPolicyHash = mintingPolicyHash fakeMintingPolicy

headValidatorHash :: ValidatorHash
headValidatorHash = Head.validatorHash fakeMintingPolicyHash

-- Setup: prepare the UTXOs and scripts in a state that is suitable for tracing.
-- Commit: consume an Initial UTXO to commit a UTXO to the Commit script.
-- Abort: consume an Initial UTXO with a Head UTXO to abort.
type InitialSchema = Endpoint "Setup" () .\/ Endpoint "Commit" () .\/ Endpoint "Abort" ()

initialEndpoints :: Contract () InitialSchema ContractError ()
initialEndpoints = selectList [s, c, a] >> initialEndpoints
  where s = endpoint @"Setup" $ \_ -> handleError logError setup
        c = endpoint @"Commit" $ \_ -> handleError logError commit
        a = endpoint @"Abort" $ \_ -> handleError logError abort

-------------------------------------------------------------------------------

setup :: Contract w s ContractError ()
setup = do
  pkh <- ownPubKeyHash
  let datumInitial =
        ( fakeMintingPolicyHash,
          Dependencies headValidatorHash Commit.validatorHash,
          pkh
        )

  -- Prepare two Initial UTXOs.
  -- One for the commit trace, one for the abort trace.
  forM_ ([1 .. 2] :: [Int]) $
    const $
      awaitTxConfirmed . getCardanoTxId
        =<< submitTxConstraintsWith
          (typedValidatorLookups typedValidator)
          (mustPayToTheScript datumInitial (Ada.lovelaceValueOf 8))

  -- Initialize a Head for the abort trace.
  awaitTxConfirmed . getCardanoTxId
    =<< submitTxConstraintsWith
      (typedValidatorLookups (Head.typedValidator fakeMintingPolicyHash))
      (mustPayToTheScript (Head.Initial 8 []) (Ada.lovelaceValueOf 8))

  -- Break down the initial UTXO to have two.
  -- One to commit away, one to pay fees and all.
  awaitTxConfirmed . getCardanoTxId
    =<< submitTx
      (mustPayToPubKey pkh (Ada.lovelaceValueOf 8888888))

-------------------------------------------------------------------------------

commit :: Contract w s ContractError ()
commit = do
  -- Find an arbitrary user UTXO to commit through an arbitrary Initial UTXO.
  pkh <- ownPubKeyHash
  (soref, so) <- head . M.toList <$> utxosAt (scriptHashAddress validatorHash)
  (uoref, uo) <- head . M.toList <$> utxosAt (pubKeyHashAddress pkh)
  let datumCommit = Commit.datum (Commit.Dependencies headValidatorHash, toTxOut uo)
      lookups =
        typedValidatorLookups Commit.typedValidator
          <> otherScript (Scripts.validatorScript typedValidator)
          <> mintingPolicy fakeMintingPolicy
          <> unspentOutputs (M.fromList [(soref, so), (uoref, uo)])
      party = mkParty fakeMintingPolicyHash pkh
      commitVal = txOutValue (toTxOut uo) <> party
      -- This shouldn't match the final wanted behavior.
      -- Just a minimal setup to run traces.
      tx =
        mustPayToOtherScript Commit.validatorHash datumCommit commitVal
          <> mustMintValue party
          <> mustSpendScriptOutput soref (redeemer (Just uoref))
          <> mustSpendPubKeyOutput uoref
  awaitTxConfirmed . getCardanoTxId =<< submitTxConstraintsWith lookups tx

-------------------------------------------------------------------------------

abort :: Contract w s ContractError ()
abort = do
  -- Abort an arbitrary Initial and Head pair of UTXOs.
  pkh <- ownPubKeyHash
  (ioref, io) <- head . M.toList <$> utxosAt (scriptHashAddress validatorHash)
  (horef, ho) <- head . M.toList <$> utxosAt (scriptHashAddress headValidatorHash)
  let lookups =
        otherScript (Scripts.validatorScript typedValidator)
          <> otherScript (Scripts.validatorScript (Head.typedValidator fakeMintingPolicyHash))
          <> unspentOutputs (M.fromList [(ioref, io), (horef, ho)])
      -- This shouldn't match the final wanted behavior.
      -- Just a minimal setup to run traces.
      tx =
        mustSpendScriptOutput ioref (redeemer Nothing)
          <> mustSpendScriptOutput horef (Redeemer (PlutusTx.toBuiltinData Head.Abort))
          <> mustPayToPubKey pkh (txOutValue (toTxOut io))
          <> mustPayToOtherScript headValidatorHash (Datum (PlutusTx.toBuiltinData Head.Final)) (txOutValue (toTxOut ho))
  awaitTxConfirmed . getCardanoTxId =<< submitTxConstraintsWith @Scripts.Any lookups tx

-------------------------------------------------------------------------------

initialTrace :: Trace.EmulatorTrace ()
initialTrace = do
  hdl <- Trace.activateContractWallet (knownWallet 1) initialEndpoints
  void $ Trace.waitNSlots 8
  Trace.callEndpoint @"Setup" hdl ()
  void $ Trace.waitNSlots 8
  Trace.callEndpoint @"Commit" hdl ()
  void $ Trace.waitNSlots 8
  Trace.callEndpoint @"Abort" hdl ()
  void $ Trace.waitNSlots 8