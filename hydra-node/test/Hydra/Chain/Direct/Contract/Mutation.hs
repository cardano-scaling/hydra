{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provides building blocks for Mutation testing of Contracts.
--
-- == Introduction
--
-- Traditional [Mutation testing](https://en.wikipedia.org/wiki/Mutation_testing) is a testing technique
-- that introduces small modifications like changing a comparison operator, or modifying constants, into
-- a program and checks whether or not the existing tests "kill" the produced mutants, eg. fail. Mutation
-- testing requires somewhat complex tooling because it needs to modify the source code, in limited and
-- semantically meaningful ways in order to generate code that won't be rejected by the compiler.
--
-- Recall that Plutus eUTxO validators are boolean expressions of the form:
--
-- @
-- validator : Datum -> Redeemer -> ScriptContext -> Bool
-- @
--
-- All things being equal, "mutating" a /validator/ so that it returns `False` instead of `True` can be done:
--
-- * Either by /mutating/ the code of the `validator` implementation,
-- * Or by /mutating/ its arguments.
--
-- This simple idea lead to the following strategy to test-drive validator scripts:
--
-- 1. Start with a validator that always return `True`,
-- 2. Write a /positive/ property test checking /valid/ transactions are accepted by the validator(s),
-- 3. Write a /negative/ property test checking /invalid/ transactions are rejected. This is where /mutations/
--    are introduced, each different mutation type representing some possible "attack",
-- 4. Watch one or the other properties fail and enhance the validators code to make them pass,
-- 5. Rinse and repeat.
--
-- == Generic Property and Mutations
--
-- Given a transaction with some UTxO context, and a function that generates `SomeMutation` from a valid
-- transaction and context pair, the generic 'propMutation' property checks applying any generated mutation
-- makes the mutated (hence expectedly invalid) transaction fail the validation stage.
--
-- @
-- propMutation :: (Tx, Utxo) -> ((Tx, Utxo) -> Gen SomeMutation) -> Property
-- propMutation (tx, utxo) genMutation =
--   forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
--     (tx, utxo)
--       & applyMutation mutation
--       & propTransactionDoesNotValidate
-- @
--
-- To this basic property definition we add a `checkCoverage` that ensures the set of generated mutations
-- covers a statistically significant share of each of the various possible mutations classified by their @label@.
--
-- The `SomeMutation` type is simply a wrapper that attaches a @label@ to a proper `Mutation` which is the interesting bit here.
--
-- The `Mutation` type enumerates various possible "atomic" mutations which preserve the structural correctness of the transaction but should make a validator fail.
--
-- @
-- data Mutation
--   = ChangeHeadRedeemer Head.Input
--   | ChangeHeadDatum Head.State
--   ...
--   | Changes [Mutation]
-- @
--
-- The constructors should hopefully be self-explaining but for the last one. Some interesting mutations we want
-- to make require more than one "atomic" change to represent a possible validator failure. For example,
-- we wanted to check that the `Commit` validator, in the context of a `CollectCom` transaction, verifies the
-- state (`Input`) of the `Head` validator is correct. But to be interesting, this mutation needs to ensure the
-- /transition/ verified by the `Head` state machine is valid, which requires changing /both/ the datum and the
-- redeemer of the consumed head output.
--
-- == Transaction-specific Mutations
--
-- To be run the `propMutation` requires a starting "healthy" (valid) transaction and a specialised generating
-- function. It is instantiated in the test runner by providing these two elements. For example, the "ContractSpec"
-- module has the following property check:
--
-- @
-- describe "CollectCom" $ do
--   prop "does not survive random adversarial mutations" $
--     propMutation healthyCollectComTx genCollectComMutation
-- @
--
-- The interesting part is the `genCollectComMutation` (details of the `Mutation` generators are omitted):
--
-- @
-- genCollectComMutation :: (Tx, Utxo) -> Gen SomeMutation
-- genCollectComMutation (tx, utxo) =
--   oneof
--     [ SomeMutation MutateOpenOutputValue . ChangeOutput ...
--     , SomeMutation MutateOpenUtxoHash . ChangeOutput ...
--     , SomeMutation MutateHeadScriptInput . ChangeInput ...
--     , SomeMutation MutateHeadTransition <$> do
--         changeRedeemer <- ChangeHeadRedeemer <$> ...
--         changeDatum <- ChangeHeadDatum <$> ...
--         pure $ Changes [changeRedeemer, changeDatum]
--     ]
-- @
--
-- Here we have defined four different type of mutations that are interesting for the "CollectCom" transaction
-- and represent possible /attack vectors/:
--
--   * Changing the `Head` output's value, which would imply some of the committed funds could be "stolen"
--     by the party posting the transaction,
--   * Tampering with the content of the UTxO committed to the Head,
--   * Trying to collect commits without running the `Head` validator,
--   * Trying to collect commits in another Head state machine transition.
--
-- == Running Properties
--
-- When such a property test succeeds we get the following report which shows the distribution of the various
-- mutations that were tested.
--
-- @
-- Hydra.Chain.Direct.Contract
--   CollectCom
--     does not survive random adversarial mutations
--       +++ OK, passed 200 tests.
--
--       CollectComMutation (200 in total):
--       30.5% MutateOpenUtxoHash
--       27.0% MutateHeadTransition
--       23.5% MutateOpenOutputValue
--       19.0% MutateHeadScriptInput
--
-- Finished in 18.1146 seconds
-- @
--
-- In the case of a failure we get a detailed report on the context of the failure.
module Hydra.Chain.Direct.Contract.Mutation where

import Hydra.Cardano.Api

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger
import Cardano.Ledger.Serialization (mkSized)
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Hydra.Chain.Direct.Fixture (genForParty, testPolicyId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.State (ChainState (..), observeSomeTx)
import Hydra.Chain.Direct.Tx (assetNameFromVerificationKey)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (genKeyPair, genOutput, genVerificationKey, renderTxWithUTxO)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (Party)
import Hydra.Prelude hiding (label)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (fromData, toData)
import qualified System.Directory.Internal.Prelude as Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (
  Property,
  checkCoverage,
  conjoin,
  counterexample,
  forAll,
  forAllBlind,
  property,
  suchThat,
  vector,
 )
import Test.QuickCheck.Instances ()

-- * Properties

-- | A 'Property' checking a mutation is not validated.
-- This property takes an initial (transaction, UTxO) pair that is supposedly valid,
-- passes it to a generator that produces some mutations, then assert the resulting
-- (transaction', UTxO') pair fails the validation process.
--
-- Note that only "level 2" validation is run, e.g the transaction is assume to be
-- structurally valid and having passed "level 1" checks.
propMutationOnChain :: (Tx, UTxO) -> ((Tx, UTxO) -> Gen SomeMutation) -> Property
propMutationOnChain (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & genericCoverTable [label]
      & checkCoverage

propMutationOffChain ::
  (Tx, UTxO) ->
  ((Tx, UTxO) -> Gen SomeMutation) ->
  Gen ChainState ->
  Property
propMutationOffChain (tx, utxo) genMutation genSt =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    forAllBlind genSt $ \st ->
      (tx, utxo)
        & applyMutation mutation
        & ( \x ->
              conjoin
                [ propTransactionValidates x
                    & counterexample "Transaction should have validated but didn't."
                , propTransactionIsNotObserved x st
                    & counterexample "Transaction should have not been observed but was observed."
                ]
          )
        & genericCoverTable [label]
        & checkCoverage

-- | A 'Property' checking some (transaction, UTxO) pair is invalid.
propTransactionDoesNotValidate :: (Tx, UTxO) -> Property
propTransactionDoesNotValidate (tx, lookupUTxO) =
  let result = evaluateTx tx lookupUTxO
   in case result of
        Left _ ->
          property True
        Right redeemerReport ->
          any isLeft (Map.elems redeemerReport)
            & counterexample ("Tx: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation should have failed"

-- | A 'Property' checking some (transaction, UTxO) pair is valid.
propTransactionValidates :: (Tx, UTxO) -> Property
propTransactionValidates (tx, lookupUTxO) =
  let result = evaluateTx tx lookupUTxO
   in case result of
        Left basicFailure ->
          property False
            & counterexample ("Tx: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation failed"

-- | A 'Property' checking some (on-chain valid) (transaction, UTxO) is not
-- properly observe given a configuration.
propTransactionIsNotObserved :: (Tx, UTxO) -> ChainState -> Property
propTransactionIsNotObserved (tx, _) st =
  case observeSomeTx tx st of
    Nothing ->
      property True
    Just (onChainTx, st') ->
      property False
        & counterexample ("Observed tx: " <> strawmanGetConstr onChainTx)
        & counterexample ("New head state: " <> show (typeOf st'))
 where
  strawmanGetConstr = toString . Prelude.head . words . show

-- * Mutations

-- | Existential wrapper 'SomeMutation' and some label type.
-- This type is useful to provide a "generic"  classification of mutation
-- that is controlled by some custom type. The 'label' field can be passed
-- to the 'genericCoverTable' function to construct and display a coverage
-- table showing the percentage of each mutation that's been applied and
-- ensure significant coverage of all possible mutations using 'checkCoverage'.
data SomeMutation = forall lbl.
  (Typeable lbl, Enum lbl, Bounded lbl, Show lbl) =>
  SomeMutation
  { label :: lbl
  , mutation :: Mutation
  }

deriving instance Show SomeMutation

-- | Basic mutations
data Mutation
  = -- | Changes the 'Head' script's redeemer to the given value.
    ChangeHeadRedeemer Head.Input
  | -- | Changes the 'Head' script's datum to the given value.
    -- This modifies both the  'DatumHash' in the UTxO context and the
    -- map of 'DatumHash' to 'Datum' in the transaction's witnesses.
    ChangeHeadDatum Head.State
  | -- | Adds given output to the transaction's outputs.
    PrependOutput (TxOut CtxTx)
  | -- | Removes given output from the transaction's outputs.
    RemoveOutput Word
  | -- | Drops the given input from the transaction's inputs
    RemoveInput TxIn
  | -- | Change an input's 'TxOut' to something else.
    -- This mutation alters the redeemers of the transaction to ensure
    -- any matching redeemer for given input matches the new redeemer, otherwise
    -- the transaction would be invalid for the wrong reason (unused redeemer).
    --
    -- This expects 'Nothing' if the new input is not locked by any script, and
    -- it expects 'Just' with some potentially new redeemer if locked by a
    -- script.
    ChangeInput TxIn (TxOut CtxUTxO) (Maybe ScriptData)
  | -- | Change the transaction's output at given index to something else.
    ChangeOutput Word (TxOut CtxTx)
  | -- | Change the transaction's minted values if it is actually minting something.
    ChangeMintedValue Value
  | -- | Change required signers on a transaction'
    ChangeRequiredSigners [Hash PaymentKey]
  | -- | Change the validity interval of the transaction.
    ChangeValidityInterval (TxValidityLowerBound, TxValidityUpperBound)
  | -- | Applies several mutations as a single atomic 'Mutation'.
    -- This is useful to enable specific mutations that require consistent
    -- change of more than one thing in the transaction and/or UTxO set, for
    -- example to change consistently the Head script's redeemer and datum.
    Changes [Mutation]
  deriving (Show, Generic)

-- | Apply a single 'Mutation' to the given (transaction, UTxO) pair.
-- '''NOTE''': This function is partial, it can raise 'error' when some preconditions
-- are not met by the transaction or UTxO set, for example if there's no
-- Head script input or no datums in the transaction.
applyMutation :: Mutation -> (Tx, UTxO) -> (Tx, UTxO)
applyMutation mutation (tx@(Tx body wits), utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    let ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
        headOutputIndices =
          fst
            <$> filter
              (isHeadOutput . snd . snd)
              (zip [0 :: Word64 ..] $ Map.toAscList $ UTxO.toMap utxo)
        headInputIdx = case headOutputIndices of
          [i] -> i
          _ -> error $ "could not find head output in utxo: " <> show utxo

        newHeadRedeemer (Ledger.RdmrPtr _ ix) (dat, units)
          | ix == headInputIdx = (Ledger.Data (toData newRedeemer), units)
          | otherwise = (dat, units)

        redeemers = alterRedeemers newHeadRedeemer scriptData
        body' = ShelleyTxBody ledgerBody scripts redeemers mAuxData scriptValidity
     in (Tx body' wits, utxo)
  ChangeHeadDatum d' ->
    let datum = mkTxOutDatum d'
        datumHash = mkTxOutDatumHash d'
        -- change the lookup UTXO
        fn o@(TxOut addr value _ refScript)
          | isHeadOutput o =
            TxOut addr value datumHash refScript
          | otherwise =
            o
        -- change the datums in the tx
        ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
        newDatums = addDatum datum scriptData
        body' = ShelleyTxBody ledgerBody scripts newDatums mAuxData scriptValidity
     in (Tx body' wits, fmap fn utxo)
  PrependOutput txOut ->
    ( alterTxOuts (txOut :) tx
    , utxo
    )
  RemoveOutput ix ->
    ( alterTxOuts (removeAt ix) tx
    , utxo
    )
   where
    removeAt i es =
      if fromIntegral i >= length es
        then error "trying to removeAt beyond end of list"
        else
          map snd $
            filter ((/= i) . fst) $ zip [0 ..] es
  RemoveInput i ->
    ( alterTxIns (safeFilter (/= i)) tx
    , utxo
    )
   where
    safeFilter fn xs =
      let xs' = filter fn xs
       in if xs' == xs
            then error "RemoveInput did not remove any input."
            else xs'
  ChangeInput txIn txOut maybeRedeemer ->
    ( Tx body' wits
    , UTxO $ Map.insert txIn txOut (UTxO.toMap utxo)
    )
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    redeemers = alterRedeemerFor (Ledger.inputs ledgerBody) (toLedgerTxIn txIn) (const maybeRedeemer) scriptData
    body' = ShelleyTxBody ledgerBody scripts redeemers mAuxData scriptValidity
  ChangeOutput ix txOut ->
    ( alterTxOuts replaceAtIndex tx
    , utxo
    )
   where
    replaceAtIndex outs =
      foldr
        ( \(i, out) list ->
            if i == ix then txOut : list else out : list
        )
        []
        (zip [0 ..] outs)
  ChangeMintedValue v' ->
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    ledgerBody' = ledgerBody{Ledger.mint = toLedgerValue v'}
    body' = ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity
  ChangeRequiredSigners newSigners ->
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' = ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity
    ledgerBody' =
      ledgerBody
        { Ledger.reqSignerHashes = Set.fromList (toLedgerKeyHash <$> newSigners)
        }
  ChangeValidityInterval (lb, up) ->
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' = ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity
    ledgerBody' =
      ledgerBody
        { Ledger.txvldt = toLedgerValidityInterval (lb, up)
        }
  Changes mutations ->
    foldr applyMutation (tx, utxo) mutations

--
-- Generators
--

genBytes :: Gen ByteString
genBytes = arbitrary

genHash :: Gen ByteString
genHash = BS.pack <$> vector 32

-- * Orphans

deriving instance Eq Head.Input

instance Arbitrary Head.Input where
  arbitrary = genericArbitrary

instance Arbitrary Head.State where
  arbitrary = genericArbitrary

-- * Helpers

-- | Identify Head script's output.
isHeadOutput :: TxOut CtxUTxO -> Bool
isHeadOutput TxOut{txOutAddress = addr} = addr == headAddress
 where
  headAddress = mkScriptAddress @PlutusScriptV2 Fixture.testNetworkId headScript
  headScript = fromPlutusScript Head.validatorScript

-- | Adds given 'Datum' and corresponding hash to the transaction's scripts.
-- TODO: As we are creating the `TxOutDatum` from a known datum, passing a `TxOutDatum` is
-- pointless and requires more work than needed to check impossible variants.
addDatum :: TxOutDatum CtxTx -> TxBodyScriptData -> TxBodyScriptData
addDatum datum scriptData =
  case datum of
    TxOutDatumNone -> error "unexpected datum none"
    TxOutDatumHash _ha -> error "hash only, expected full datum"
    TxOutDatumInline _sd -> error "not useful for inline datums"
    TxOutDatumInTx sd ->
      case scriptData of
        TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
        TxBodyScriptData (Ledger.TxDats dats) redeemers ->
          let dat = toLedgerData sd
              newDats = Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats
           in TxBodyScriptData newDats redeemers

changeHeadOutputDatum :: (Head.State -> Head.State) -> TxOut CtxTx -> TxOut CtxTx
changeHeadOutputDatum fn txOut =
  case txOutDatum txOut of
    TxOutDatumNone ->
      error "Unexpected empty head datum"
    (TxOutDatumHash _ha) ->
      error "Unexpected hash-only datum"
    (TxOutDatumInline _sd) ->
      error "Unexpected inlined datum"
    (TxOutDatumInTx sd) ->
      case fromData $ toPlutusData sd of
        Just st ->
          txOut{txOutDatum = mkTxOutDatum $ fn st}
        Nothing ->
          error "Invalid data"

addParticipationTokens :: [Party] -> TxOut CtxUTxO -> TxOut CtxUTxO
addParticipationTokens parties txOut =
  txOut{txOutValue = val'}
 where
  val' =
    txOutValue txOut
      <> valueFromList
        [ (AssetId testPolicyId (assetNameFromVerificationKey cardanoVk), 1)
        | cardanoVk <- genForParty genVerificationKey <$> parties
        ]

-- | Ensures the included datums of given 'TxOut's are included in the transactions' 'TxBodyScriptData'.
ensureDatums :: [TxOut CtxTx] -> TxBodyScriptData -> TxBodyScriptData
ensureDatums outs scriptData =
  foldr ensureDatum scriptData outs
 where
  ensureDatum txOut sd =
    case txOutDatum txOut of
      d@(TxOutDatumInTx _) -> addDatum d sd
      _ -> sd

-- | Alter a transaction's  redeemers map given some mapping function.
alterRedeemers ::
  ( Ledger.RdmrPtr ->
    (Ledger.Data LedgerEra, Ledger.ExUnits) ->
    (Ledger.Data LedgerEra, Ledger.ExUnits)
  ) ->
  TxBodyScriptData ->
  TxBodyScriptData
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData dats (Ledger.Redeemers redeemers) ->
    let newRedeemers = Map.mapWithKey fn redeemers
     in TxBodyScriptData dats (Ledger.Redeemers newRedeemers)

-- | Remove redeemer for given `TxIn` from the transaction's redeemers map.
alterRedeemerFor ::
  Set (Ledger.TxIn a) ->
  Ledger.TxIn a ->
  (ScriptData -> Maybe ScriptData) ->
  TxBodyScriptData ->
  TxBodyScriptData
alterRedeemerFor initialInputs txIn fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData dats (Ledger.Redeemers initialRedeemers) ->
    let newRedeemers = Ledger.Redeemers $ Map.fromList $ foldMap removeRedeemer $ Map.toList initialRedeemers
        sortedInputs = sort $ toList initialInputs
        removeRedeemer (ptr@(Ledger.RdmrPtr _ idx), (sd, exUnits))
          | sortedInputs List.!! fromIntegral idx == txIn =
            case fn (fromLedgerData sd) of
              Nothing -> []
              Just sd' -> [(ptr, (toLedgerData sd', exUnits))]
          | otherwise = [(ptr, (sd, exUnits))]
     in TxBodyScriptData dats newRedeemers

alterTxIns ::
  ([TxIn] -> [TxIn]) ->
  Tx ->
  Tx
alterTxIns fn (Tx body wits) =
  Tx (ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity) wits
 where
  ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
  inputs' = fn . fmap fromLedgerTxIn . toList $ Ledger.inputs ledgerBody
  ledgerBody' =
    ledgerBody
      { Ledger.inputs = Set.fromList (toLedgerTxIn <$> inputs')
      }

-- | Apply some mapping function over a transaction's outputs.
alterTxOuts ::
  ([TxOut CtxTx] -> [TxOut CtxTx]) ->
  Tx ->
  Tx
alterTxOuts fn tx =
  Tx body' wits
 where
  body' = ShelleyTxBody ledgerBody' scripts scriptData' mAuxData scriptValidity
  ledgerBody' = ledgerBody{Ledger.outputs = ledgerOutputs'}

  ledgerOutputs' = StrictSeq.fromList . map (mkSized . toLedgerTxOut . toCtxUTxOTxOut) $ outputs'

  outputs' = fn . fmap fromLedgerTxOut . toList $ Ledger.outputs' ledgerBody

  scriptData' = ensureDatums outputs' scriptData

  ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
  Tx body wits = tx

-- | Generates an output that pays to some arbitrary pubkey.
anyPayToPubKeyTxOut :: Gen (TxOut ctx)
anyPayToPubKeyTxOut = genKeyPair >>= genOutput . fst

-- | Finds the Head script's input in given `UTxO` set.
-- '''NOTE''': This function is partial, it assumes the `UTxO` set contains a
-- Head script output.
headTxIn :: UTxO -> TxIn
headTxIn = fst . Prelude.head . filter (isHeadOutput . snd) . UTxO.pairs

-- | A 'Mutation' that changes the minted/burnt quantity of all tokens.
changeMintedValueQuantityFrom :: Tx -> Integer -> Gen Mutation
changeMintedValueQuantityFrom tx exclude =
  ChangeMintedValue
    <$> case mintedValue of
      TxMintValueNone ->
        pure mempty
      TxMintValue v _ -> do
        someQuantity <- fromInteger <$> arbitrary `suchThat` (/= exclude)
        pure . valueFromList $ map (second $ const someQuantity) $ valueToList v
 where
  mintedValue = txMintValue $ txBodyContent $ txBody tx

-- | A `Mutation` that adds an `Arbitrary` participation token with some quantity.
-- As usual the quantity can be positive for minting, or negative for burning.
addPTWithQuantity :: Tx -> Quantity -> Gen Mutation
addPTWithQuantity tx quantity =
  ChangeMintedValue <$> do
    case mintedValue of
      TxMintValue v _ -> do
        -- NOTE: We do not expect Ada or any other assets to be minted, so
        -- we can take the policy id from the headtake the policy id from
        -- the head.
        case Prelude.head $ valueToList v of
          (AdaAssetId, _) -> error "unexpected mint of Ada"
          (AssetId pid _an, _) -> do
            -- Some arbitrary token name, which could correspond to a pub key hash
            pkh <- arbitrary
            pure $ v <> valueFromList [(AssetId pid pkh, quantity)]
      TxMintValueNone ->
        pure mempty
 where
  mintedValue = txMintValue $ txBodyContent $ txBody tx
