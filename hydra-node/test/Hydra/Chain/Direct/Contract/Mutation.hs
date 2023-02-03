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
--   | ChangeInputHeadDatum Head.State
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
--     [ SomeMutation Nothing MutateOpenOutputValue . ChangeOutput ...
--     , SomeMutation Nothing MutateOpenUtxoHash . ChangeOutput ...
--     , SomeMutation Nothing MutateHeadTransition <$> do
--         changeRedeemer <- ChangeHeadRedeemer <$> ...
--         changeDatum <- ChangeInputHeadDatum <$> ...
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
--       CollectComMutation (100 in total):
--       23% MutateNumberOfParties
--       22% MutateHeadTransition
--       21% MutateHeadId
--       19% MutateOpenUTxOHash
--       15% MutateRequiredSigner
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Fixture (testPolicyId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (assetNameFromVerificationKey)
import Hydra.Chain.Direct.Util (addDatum, alterTxOuts)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Data.Party as Data (Party)
import Hydra.Ledger.Cardano (genKeyPair, genOutput, genVerificationKey, renderTxWithUTxO)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (Party)
import Hydra.Prelude hiding (label)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (CurrencySymbol, POSIXTime, fromData, toData)
import qualified System.Directory.Internal.Prelude as Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (
  Property,
  checkCoverage,
  counterexample,
  forAll,
  property,
  suchThat,
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
propMutation :: (Tx, UTxO) -> ((Tx, UTxO) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation, expectedError} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate expectedError
      & genericCoverTable [label]
      & checkCoverage

-- | A 'Property' checking some (transaction, UTxO) pair is invalid.
propTransactionDoesNotValidate :: Maybe Text -> (Tx, UTxO) -> Property
propTransactionDoesNotValidate mExpectedError (tx, lookupUTxO) =
  let result = evaluateTx tx lookupUTxO
   in case result of
        Left basicFailure ->
          property False
            & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          let errors = lefts $ Map.elems redeemerReport
           in case mExpectedError of
                Nothing ->
                  not (null errors)
                    & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample "Phase-2 validation should have failed"
                Just expectedError ->
                  any (matchesErrorMessage expectedError) errors
                    & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample ("Phase-2 validation should have failed with error message: " <> show expectedError)
 where
  matchesErrorMessage errMsg = \case
    ScriptErrorEvaluationFailed _ errList -> errMsg `elem` errList
    _otherScriptExecutionError -> False

-- | A 'Property' checking some (transaction, UTxO) pair is valid.
propTransactionValidates :: (Tx, UTxO) -> Property
propTransactionValidates (tx, lookupUTxO) =
  let result = evaluateTx tx lookupUTxO
   in case result of
        Left basicFailure ->
          property False
            & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation failed"

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
  { expectedError :: Maybe Text
  , label :: lbl
  , mutation :: Mutation
  }

deriving instance Show SomeMutation

-- | Basic mutations
data Mutation
  = -- | Changes the 'Head' script's redeemer to the given value.
    ChangeHeadRedeemer Head.Input
  | -- | Changes the spent 'Head' script datum to the given value. This modifies
    -- both the 'DatumHash' in the UTxO context and the map of 'DatumHash' to
    -- 'Datum' in the transaction's witnesses.
    ChangeInputHeadDatum Head.State
  | -- | Adds given output to the transaction's outputs.
    PrependOutput (TxOut CtxTx)
  | -- | Removes given output from the transaction's outputs.
    RemoveOutput Word
  | -- | Drops the given input from the transaction's inputs
    RemoveInput TxIn
  | -- | Adds given UTxO to the transaction's inputs and UTxO context.
    AddInput TxIn (TxOut CtxUTxO) (Maybe ScriptData)
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
  | -- | Change the transaction's minted values if it is actually minting
    -- something. NOTE: If 'Value' is 'mempty' the redeemers will be wrong.
    ChangeMintedValue Value
  | -- | Change required signers on a transaction'
    ChangeRequiredSigners [Hash PaymentKey]
  | -- | Change the validity interval of the transaction.
    ChangeValidityInterval (TxValidityLowerBound, TxValidityUpperBound)
  | ChangeValidityLowerBound TxValidityLowerBound
  | ChangeValidityUpperBound TxValidityUpperBound
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
  ChangeInputHeadDatum d' ->
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
  RemoveInput txIn ->
    ( alterTxIns (filter (\(i, _) -> i /= txIn)) tx
    , utxo
    )
  AddInput i o newRedeemer ->
    ( alterTxIns addRedeemer tx
    , UTxO $ Map.insert i o (UTxO.toMap utxo)
    )
   where
    addRedeemer =
      map $ \(txIn', mRedeemer) ->
        if txIn' == i then (i, newRedeemer) else (txIn', mRedeemer)
  ChangeInput txIn txOut newRedeemer ->
    ( alterTxIns replaceRedeemer tx
    , UTxO $ Map.insert txIn txOut (UTxO.toMap utxo)
    )
   where
    replaceRedeemer =
      map $ \(txIn', mRedeemer) ->
        if txIn' == txIn then (txIn, newRedeemer) else (txIn', mRedeemer)
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
  ChangeValidityInterval (lowerBound, upperBound) ->
    changeValidityInterval (Just lowerBound) (Just upperBound)
  ChangeValidityLowerBound bound ->
    changeValidityInterval (Just bound) Nothing
  ChangeValidityUpperBound bound ->
    changeValidityInterval Nothing (Just bound)
  Changes mutations ->
    foldr applyMutation (tx, utxo) mutations
 where
  changeValidityInterval lowerBound' upperBound' =
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' = ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity
    ledgerBody' =
      ledgerBody
        { Ledger.txvldt =
            toLedgerValidityInterval
              ( fromMaybe lowerBound lowerBound'
              , fromMaybe upperBound upperBound'
              )
        }
    (lowerBound, upperBound) = fromLedgerValidityInterval ledgerValidityInterval
    ledgerValidityInterval = Ledger.txvldt ledgerBody

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

-- | Alter the tx inputs in such way that redeemer pointer stay consistent. A
-- value of 'Nothing' for the redeemr means that this is not a script input.
-- NOTE: This will reset all the execution budgets to 0.
alterTxIns ::
  ([(TxIn, Maybe ScriptData)] -> [(TxIn, Maybe ScriptData)]) ->
  Tx ->
  Tx
alterTxIns fn tx =
  Tx body' wits
 where
  body' = ShelleyTxBody ledgerBody' scripts scriptData' mAuxData scriptValidity

  ledgerBody' = ledgerBody{Ledger.inputs = inputs'}

  inputs' = Set.fromList $ toLedgerTxIn . fst <$> newSortedInputs

  scriptData' = TxBodyScriptData dats redeemers'

  redeemers' = Ledger.Redeemers $ rebuiltSpendingRedeemers <> nonSpendingRedeemers

  nonSpendingRedeemers =
    Map.filterWithKey (\(Ledger.RdmrPtr tag _) _ -> tag /= Ledger.Spend) redeemersMap

  rebuiltSpendingRedeemers = Map.fromList $
    flip mapMaybe (zip [0 ..] newSortedInputs) $ \(i, (_, mRedeemer)) ->
      mRedeemer <&> \d ->
        (Ledger.RdmrPtr Ledger.Spend i, (toLedgerData d, Ledger.ExUnits 0 0))

  -- NOTE: This needs to be ordered, such that we can calculate the redeemer
  -- pointers correctly.
  newSortedInputs =
    sortOn fst $
      fn
        . resolveRedeemers
        . fmap fromLedgerTxIn
        . toList
        $ Ledger.inputs ledgerBody

  resolveRedeemers :: [TxIn] -> [(TxIn, Maybe ScriptData)]
  resolveRedeemers txInputs =
    zip txInputs [0 ..] <&> \(txIn, i) ->
      case Map.lookup (Ledger.RdmrPtr Ledger.Spend i) redeemersMap of
        Nothing -> (txIn, Nothing)
        Just (redeemerData, _exUnits) -> (txIn, Just $ fromLedgerData redeemerData)

  (dats, redeemersMap) = case scriptData of
    TxBodyNoScriptData -> (mempty, mempty)
    TxBodyScriptData d (Ledger.Redeemers r) -> (d, r)

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

-- | A 'Mutation' that changes the minted/burnt quantity of all tokens to a
-- non-zero value different than the given one.
changeMintedValueQuantityFrom :: Tx -> Integer -> Gen Mutation
changeMintedValueQuantityFrom tx exclude =
  ChangeMintedValue
    <$> case mintedValue of
      TxMintValueNone ->
        pure mempty
      TxMintValue v _ -> do
        someQuantity <- fromInteger <$> arbitrary `suchThat` (/= exclude) `suchThat` (/= 0)
        pure . valueFromList $ map (second $ const someQuantity) $ valueToList v
 where
  mintedValue = txMintValue $ txBodyContent $ txBody tx

-- | A 'Mutation' that changes the minted/burned quantity of tokens like this:
-- - when no value is being minted/burned -> add a value
-- - when tx is minting or burning values -> add more values on top of that
changeMintedTokens :: Tx -> Value -> Gen Mutation
changeMintedTokens tx mintValue =
  ChangeMintedValue
    <$> case mintedValue of
      TxMintValueNone ->
        pure mintValue
      TxMintValue v _ ->
        pure $ v <> mintValue
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
        -- we can take the policy id from the head
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

-- | Replace original policy id with the arbitrary one
replacePolicyIdWith :: PolicyId -> PolicyId -> TxOut a -> TxOut a
replacePolicyIdWith originalPolicyId otherPolicyId output =
  let value = txOutValue output
      newValue = valueFromList $ swapPolicyId <$> valueToList value
      swapPolicyId = \case
        (AssetId policyId t, q) | policyId == originalPolicyId -> (AssetId otherPolicyId t, q)
        v -> v
   in output{txOutValue = newValue}

replaceSnapshotNumber :: Head.SnapshotNumber -> Head.State -> Head.State
replaceSnapshotNumber snapshotNumber = \case
  Head.Closed{parties, utxoHash, contestationDeadline, headId} ->
    Head.Closed
      { Head.parties = parties
      , Head.snapshotNumber = snapshotNumber
      , Head.utxoHash = utxoHash
      , Head.contestationDeadline = contestationDeadline
      , Head.headId = headId
      }
  otherState -> otherState

replaceParties :: [Data.Party] -> Head.State -> Head.State
replaceParties parties = \case
  Head.Initial{contestationPeriod, headId} ->
    Head.Initial
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.headId = headId
      }
  Head.Open{contestationPeriod, utxoHash, headId} ->
    Head.Open
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.utxoHash = utxoHash
      , Head.headId = headId
      }
  Head.Closed{snapshotNumber, utxoHash, contestationDeadline, headId} ->
    Head.Closed
      { Head.parties = parties
      , Head.snapshotNumber = snapshotNumber
      , Head.utxoHash = utxoHash
      , Head.contestationDeadline = contestationDeadline
      , Head.headId = headId
      }
  otherState -> otherState

replaceUtxoHash :: Head.Hash -> Head.State -> Head.State
replaceUtxoHash utxoHash = \case
  Head.Open{contestationPeriod, parties, headId} ->
    Head.Open
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.utxoHash = utxoHash
      , Head.headId = headId
      }
  Head.Closed{parties, snapshotNumber, contestationDeadline, headId} ->
    Head.Closed
      { Head.parties = parties
      , Head.snapshotNumber = snapshotNumber
      , Head.utxoHash = utxoHash
      , Head.contestationDeadline = contestationDeadline
      , Head.headId = headId
      }
  otherState -> otherState

replaceContestationDeadline :: POSIXTime -> Head.State -> Head.State
replaceContestationDeadline contestationDeadline = \case
  Head.Closed{snapshotNumber, utxoHash, parties, headId} ->
    Head.Closed
      { snapshotNumber
      , utxoHash
      , parties
      , contestationDeadline
      , headId
      }
  otherState -> otherState

replaceHeadId :: CurrencySymbol -> Head.State -> Head.State
replaceHeadId headId = \case
  Head.Initial{contestationPeriod, parties} ->
    Head.Initial
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.headId = headId
      }
  Head.Open{contestationPeriod, utxoHash, parties} ->
    Head.Open
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.utxoHash = utxoHash
      , Head.headId = headId
      }
  Head.Closed{snapshotNumber, utxoHash, contestationDeadline, parties} ->
    Head.Closed
      { Head.parties = parties
      , Head.snapshotNumber = snapshotNumber
      , Head.utxoHash = utxoHash
      , Head.contestationDeadline = contestationDeadline
      , Head.headId = headId
      }
  otherState -> otherState
