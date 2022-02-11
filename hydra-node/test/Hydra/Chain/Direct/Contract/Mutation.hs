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

import Hydra.Cardano.Api hiding (SigningKey)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (policyId)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (genKeyPair, genOutput)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (SigningKey, generateKey)
import Hydra.Prelude hiding (label)
import Plutus.Orphans ()
import qualified System.Directory.Internal.Prelude as Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  counterexample,
  forAll,
  property,
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
propMutation :: (Tx, UTxO) -> ((Tx, UTxO) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
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
            & counterexample ("Tx: " <> toString (renderTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation should have failed"

-- | A 'Property' checking some (transaction, UTxO) pair is valid.
propTransactionValidates :: (Tx, UTxO) -> Property
propTransactionValidates (tx, lookupUTxO) =
  let result = evaluateTx tx lookupUTxO
   in case result of
        Left basicFailure ->
          property False
            & counterexample ("Tx: " <> toString (renderTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> toString (renderTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUTxO))
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
  | -- | Change an input's 'TxOut' to something else.
    -- This mutation alters the redeemers of the transaction to ensure
    -- any matching redeemer for given input is removed, otherwise the
    -- transaction would be invalid for the wrong reason (unused redeemer).
    --
    -- NOTE: The changed output should not be spending a script address as
    -- we don't provide any redeemer for it.
    ChangeInput TxIn (TxOut CtxUTxO)
  | -- | Change the transaction's output at given index to something else.
    ChangeOutput Word (TxOut CtxTx)
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
applyMutation mutation (tx, utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    let headOutputIndices =
          fst
            <$> filter
              (isHeadOutput . snd . snd)
              (zip [0 :: Word64 ..] $ Map.toAscList $ UTxO.toMap utxo)
        headInputIdx = case headOutputIndices of
          [i] -> i
          _ -> error $ "could not find head output in utxo: " <> show utxo

        newHeadRedeemer (Ledger.RdmrPtr _ ix) (dat, units)
          | ix == headInputIdx = (toScriptData newRedeemer, units)
          | otherwise = (dat, units)
     in (adjustRedeemers newHeadRedeemer tx, utxo)
  ChangeHeadDatum d' ->
    let -- change the lookup UTXO
        fn o@(TxOut addr value _)
          | isHeadOutput o =
            TxOut addr value (mkTxOutDatumHash d')
          | otherwise =
            o
     in (addDatum (toScriptData d') tx, fmap fn utxo)
  PrependOutput txOut ->
    ( adjustTxOuts (txOut :) tx
    , utxo
    )
  RemoveOutput ix ->
    ( adjustTxOuts (removeAt ix) tx
    , utxo
    )
   where
    removeAt i es =
      map snd $
        filter ((/= i) . fst) $ zip [0 ..] es
  ChangeInput txIn txOut ->
    ( removeRedeemerSpending txIn tx
    , UTxO $ Map.insert txIn txOut (UTxO.toMap utxo)
    )
  ChangeOutput ix txOut ->
    ( adjustTxOuts replaceAtIndex tx
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
  Changes mutations ->
    foldr applyMutation (tx, utxo) mutations

--
-- Generators
--

genListOfSigningKeys :: Gen [SigningKey]
genListOfSigningKeys = choose (1, 20) <&> fmap generateKey . enumFromTo 1

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
-- TODO: Parameterise by 'MonetaryPolicyId' as this is currently hardwired.
isHeadOutput :: TxOut CtxUTxO -> Bool
isHeadOutput (TxOut addr _ _) = addr == headAddress
 where
  headAddress = mkScriptAddress @PlutusScriptV1 Fixture.testNetworkId headScript
  headScript = fromPlutusScript $ Head.validatorScript policyId

-- | Generates an output that pays to some arbitrary pubkey.
anyPayToPubKeyTxOut :: Gen (TxOut ctx)
anyPayToPubKeyTxOut = genKeyPair >>= genOutput . fst

-- | Finds the Head script's input in given `UTxO` set.
-- '''NOTE''': This function is partial, it assumes the `UTxO` set contains a
-- Head script output.
headTxIn :: UTxO -> TxIn
headTxIn = fst . Prelude.head . filter (isHeadOutput . snd) . UTxO.pairs
