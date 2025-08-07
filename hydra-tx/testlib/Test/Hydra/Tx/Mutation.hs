{-# LANGUAGE DuplicateRecordFields #-}
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
--       & propTransactionFailsPhase2
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
module Test.Hydra.Tx.Mutation where

import Hydra.Cardano.Api

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (AllegraEraTxBody (vldtTxBodyL), AsIx (..), inputsTxBodyL, mintTxBodyL, outputsTxBodyL, reqSignerHashesTxBodyL)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Mary.Value qualified as Ledger
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Control.Exception (assert)
import Control.Lens (set, view, (.~), (^.))
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod
import Hydra.Data.Party qualified as Data (Party)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Plutus.Orphans ()
import Hydra.Prelude hiding (label, toList)
import Hydra.Tx.Utils (findFirst, onChainIdToAssetName, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, toData)
import PlutusLedgerApi.V3 qualified as Plutus
import System.Directory.Internal.Prelude qualified as Prelude
import System.Random (newStdGen, randomR)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (testPolicyId)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen ()
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
  forAll (genMutation (tx, utxo)) $ \SomeMutation{label, mutation, expectedErrors} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionFailsPhase2 expectedErrors
      & genericCoverTable [label]
      & checkCoverage

-- | Expect a phase-2 evaluation failure of given 'Tx' and 'UTxO'.
propTransactionFailsPhase2 :: [Text] -> (Tx, UTxO) -> Property
propTransactionFailsPhase2 mExpectedError (tx, lookupUTxO) =
  case evaluateTx tx lookupUTxO of
    Left err ->
      property False
        & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Phase-1 validation failed: " <> show err)
    Right redeemerReport ->
      let errors = lefts $ Map.elems redeemerReport
       in case mExpectedError of
            [] ->
              not (null errors)
                & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
                & counterexample ("Redeemer report: " <> show redeemerReport)
                & counterexample "Phase-2 validation should have failed"
            expectedErrors ->
              any (\x -> any (`matchesErrorMessage` x) expectedErrors) errors
                & counterexample ("Mutated transaction: " <> renderTxWithUTxO lookupUTxO tx)
                & counterexample ("Redeemer report: " <> show redeemerReport)
                & counterexample ("Phase-2 validation should have failed with one of error messages: " <> show expectedErrors)
 where
  matchesErrorMessage :: Text -> ScriptExecutionError -> Bool
  matchesErrorMessage errMsg = \case
    ScriptErrorEvaluationFailed (DebugPlutusFailure{dpfExecutionLogs}) -> errMsg `elem` dpfExecutionLogs
    _otherScriptExecutionError -> False

-- * Mutations

-- | Existential wrapper 'SomeMutation' and some label type.
-- This type is useful to provide a "generic"  classification of mutation
-- that is controlled by some custom type. The 'label' field can be passed
-- to the 'genericCoverTable' function to construct and display a coverage
-- table showing the percentage of each mutation that's been applied and
-- ensure significant coverage of all possible mutations using 'checkCoverage'.
data SomeMutation
  = forall lbl.
  (Typeable lbl, Enum lbl, Bounded lbl, Show lbl) =>
  SomeMutation
  { expectedErrors :: [Text]
  , label :: lbl
  , mutation :: Mutation
  }

deriving stock instance Show SomeMutation

-- | Basic mutations
data Mutation
  = -- | Changes the 'Head' script's redeemer to the given value.
    ChangeHeadRedeemer Head.Input
  | -- | Changes the spent 'Head' script datum to the given value. This assumes
    -- inline datums are used and replaces the datum in all matching UTxOs
    -- (there should only be one head txOut).
    ChangeInputHeadDatum Head.State
  | -- | Adds given output as first transaction output.
    PrependOutput (TxOut CtxTx)
  | -- | Adds given output as last transaction output.
    AppendOutput (TxOut CtxTx)
  | -- | Removes given output from the transaction's outputs.
    RemoveOutput Word
  | -- | Drops the given input from the transaction's inputs
    RemoveInput TxIn
  | -- | Adds given UTxO to the transaction's inputs and UTxO context.
    AddInput TxIn (TxOut CtxUTxO) (Maybe HashableScriptData)
  | -- | Add witness for the given script to the transaction.
    AddScript PlutusScript
  | -- | Change an input's 'TxOut' to something else.
    -- This mutation alters the redeemers of the transaction to ensure
    -- any matching redeemer for given input matches the new redeemer, otherwise
    -- the transaction would be invalid for the wrong reason (unused redeemer).
    --
    -- This expects 'Nothing' if the new input is not locked by any script, and
    -- it expects 'Just' with some potentially new redeemer if locked by a
    -- script.
    --
    -- XXX: This is super tricky to use. If passing 'Nothing' although it's a
    -- script tx out, the validator will actually not be run!
    --
    -- XXX: Likely incomplete as it can not add the datum for given txout.
    ChangeInput TxIn (TxOut CtxUTxO) (Maybe HashableScriptData)
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
  | -- | Change the included minting policy (the first minted policy) and update
    -- minted/burned values of this policy.
    ChangeMintingPolicy PlutusScript
  | -- | Applies several mutations as a single atomic 'Mutation'.
    -- This is useful to enable specific mutations that require consistent
    -- change of more than one thing in the transaction and/or UTxO set, for
    -- example to change consistently the Head script's redeemer and datum.
    Changes [Mutation]
  deriving stock (Show, Generic)

-- | Apply a single 'Mutation' to the given (transaction, UTxO) pair.
-- '''NOTE''': This function is partial, it can raise 'error' when some preconditions
-- are not met by the transaction or UTxO set, for example if there's no
-- Head script input or no datums in the transaction.
applyMutation :: Mutation -> (Tx, UTxO) -> (Tx, UTxO)
applyMutation mutation (tx@(Tx body wits), utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    (Tx body' wits, utxo)
   where
    body' = ShelleyTxBody ledgerBody scripts redeemers' mAuxData scriptValidity

    redeemers' = alterRedeemers newHeadRedeemer scriptData

    newHeadRedeemer ix (dat, units)
      | isHeadOutput (resolveInput ix) = (Ledger.Data (toData newRedeemer), units)
      | otherwise = (dat, units)

    resolveInput :: ConwayPlutusPurpose AsIx w -> TxOut CtxUTxO
    resolveInput ix =
      let k = case ix of
            ConwaySpending i -> unAsIx i
            ConwayCertifying i -> unAsIx i
            ConwayRewarding i -> unAsIx i
            ConwayMinting i -> unAsIx i
            ConwayVoting i -> unAsIx i
            ConwayProposing i -> unAsIx i
          txIn = Set.elemAt (fromIntegral k) ledgerInputs -- NOTE: calls 'error' if out of bounds
       in case UTxO.resolveTxIn (fromLedgerTxIn txIn) utxo of
            Nothing -> error $ "txIn not resolvable: " <> show txIn
            Just o -> o

    ledgerInputs = view inputsTxBodyL ledgerBody

    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
  ChangeInputHeadDatum d' ->
    ( tx
    , UTxO.map replaceHeadDatum utxo
    )
   where
    replaceHeadDatum o@(TxOut addr value _ refScript)
      | isHeadOutput o =
          TxOut addr value (mkTxOutDatumInline d') refScript
      | otherwise = o
  PrependOutput txOut ->
    ( alterTxOuts (txOut :) tx
    , utxo
    )
  AppendOutput txOut ->
    ( alterTxOuts (<> [txOut]) tx
    , utxo
    )
  RemoveOutput ix ->
    ( alterTxOuts (removeAt ix) tx
    , utxo
    )
   where
    removeAt :: Integral i => i -> [a] -> [a]
    removeAt i es =
      if fromIntegral i >= length es
        then error "trying to removeAt beyond end of list"
        else
          map snd $
            filter ((/= i) . fst) $
              zip [0 ..] es
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
  AddScript script ->
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' = ShelleyTxBody ledgerBody scripts' scriptData mAuxData scriptValidity
    scripts' = scripts <> [toLedgerScript script]
  ChangeInput txIn txOut newRedeemer ->
    assert
      redeemerGivenIfScriptTxOut
      ( alterTxIns replaceRedeemer tx
      , UTxO $ Map.insert txIn txOut (UTxO.toMap utxo)
      )
   where
    redeemerGivenIfScriptTxOut =
      not isScriptOutput || isJust newRedeemer

    isScriptOutput = case txOutAddress txOut of
      ByronAddressInEra ByronAddress{} -> False
      ShelleyAddressInEra (ShelleyAddress _ cred _) ->
        case cred of
          KeyHashObj{} -> False
          ScriptHashObj{} -> True

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
    valueToMultiAsset (Ledger.MaryValue _ multiAsset) = multiAsset
    ledgerBody' =
      ledgerBody
        & set mintTxBodyL (valueToMultiAsset $ toLedgerValue v')
    body' = ShelleyTxBody ledgerBody' scripts scriptData' mAuxData scriptValidity
    -- Drop all Mint redeemer pointers when we don't mint/burn anymore
    scriptData' =
      if v' /= mempty
        then scriptData
        else case scriptData of
          TxBodyNoScriptData -> TxBodyNoScriptData
          TxBodyScriptData dats (Ledger.Redeemers redeemers) ->
            let newRedeemers =
                  Map.filterWithKey
                    ( \x _ -> case x of
                        ConwayMinting _ -> False
                        _ -> True
                    )
                    redeemers
             in TxBodyScriptData dats (Ledger.Redeemers newRedeemers)
  ChangeRequiredSigners newSigners ->
    (Tx body' wits, utxo)
   where
    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
    body' = ShelleyTxBody ledgerBody' scripts scriptData mAuxData scriptValidity
    ledgerBody' =
      ledgerBody
        & set reqSignerHashesTxBodyL (Set.fromList (toLedgerKeyHash <$> newSigners))
  ChangeValidityInterval (lowerBound, upperBound) ->
    changeValidityInterval (Just lowerBound) (Just upperBound)
  ChangeValidityLowerBound bound ->
    changeValidityInterval (Just bound) Nothing
  ChangeValidityUpperBound bound ->
    changeValidityInterval Nothing (Just bound)
  ChangeMintingPolicy pScript ->
    ( Tx body' wits
    , utxo
    )
   where
    mutatedPid = scriptPolicyId $ PlutusScript pScript

    body' = ShelleyTxBody ledgerBody' scripts' scriptData mAuxData scriptValidity

    valueToMultiAsset (Ledger.MaryValue _ multiAsset) = multiAsset

    ledgerBody' =
      ledgerBody
        & set
          mintTxBodyL
          ( valueToMultiAsset . toLedgerValue $
              replacePolicyInValue selectedPid mutatedPid mint
          )

    selectedPid =
      fromMaybe (error "cannot mutate non minting transaction")
        . findFirst
          ( \case
              (AssetId pid _, _) -> Just pid
              (AdaAssetId, _) -> Nothing
          )
        $ toList mint

    mint = fromLedgerMultiAsset $ view mintTxBodyL ledgerBody

    scripts' =
      map
        ( \s ->
            if Ledger.PolicyID (Ledger.hashScript @LedgerEra s) == toLedgerPolicyID selectedPid
              then toLedgerScript pScript
              else s
        )
        scripts

    ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
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
        & vldtTxBodyL
          .~ toLedgerValidityInterval
            ( fromMaybe lowerBound lowerBound'
            , fromMaybe upperBound upperBound'
            )
    (lowerBound, upperBound) = fromLedgerValidityInterval ledgerValidityInterval
    ledgerValidityInterval = ledgerBody ^. vldtTxBodyL

-- * Orphans

deriving stock instance Eq Head.Input
deriving stock instance Eq Head.IncrementRedeemer
deriving stock instance Eq Head.DecrementRedeemer
deriving stock instance Eq Head.CloseRedeemer
deriving stock instance Eq Head.ContestRedeemer

-- * Helpers

-- | Identify Head script's output.
isHeadOutput :: TxOut CtxUTxO -> Bool
isHeadOutput TxOut{txOutAddress = addr} = addr == headAddress
 where
  headAddress = mkScriptAddress Fixture.testNetworkId Head.validatorScript

-- | Adds given 'Datum' and corresponding hash to the transaction's scripts.
-- TODO: As we are creating the `TxOutDatum` from a known datum, passing a `TxOutDatum` is
-- pointless and requires more work than needed to check impossible variants.
addDatum :: TxOutDatum CtxTx -> TxBodyScriptData -> TxBodyScriptData
addDatum datum scriptData =
  case datum of
    TxOutDatumNone -> error "unexpected datum none"
    TxOutDatumHash _ha -> error "hash only, expected full datum"
    TxOutDatumInline _sd -> error "not useful for inline datums"
    TxOutSupplementalDatum sd ->
      case scriptData of
        TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
        TxBodyScriptData (Ledger.TxDats dats) redeemers ->
          let dat = toLedgerData sd
              newDats = Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats
           in TxBodyScriptData newDats redeemers

modifyInlineDatum :: (FromScriptData a, ToScriptData a) => (a -> a) -> TxOut CtxTx -> TxOut CtxTx
modifyInlineDatum fn txOut =
  case txOutDatum txOut of
    TxOutDatumNone ->
      error "Unexpected empty head datum"
    (TxOutDatumHash _ha) ->
      error "Unexpected hash-only datum"
    (TxOutSupplementalDatum _sd) ->
      error "Unexpected supplemental datum"
    (TxOutDatumInline sd) ->
      case fromScriptData sd of
        Just st ->
          txOut{txOutDatum = mkTxOutDatumInline $ fn st}
        Nothing -> error "invalid data"

addParticipationTokens :: [VerificationKey PaymentKey] -> TxOut CtxUTxO -> TxOut CtxUTxO
addParticipationTokens vks txOut =
  txOut{txOutValue = val'}
 where
  val' =
    txOutValue txOut
      <> fromList
        [ (AssetId testPolicyId (onChainIdToAssetName oid), 1)
        | oid <- participants
        ]

  participants = verificationKeyToOnChainId <$> vks

-- | Ensures the included datums of given 'TxOut's are included in the transactions' 'TxBodyScriptData'.
ensureDatums :: [TxOut CtxTx] -> TxBodyScriptData -> TxBodyScriptData
ensureDatums outs scriptData =
  foldr ensureDatum scriptData outs
 where
  ensureDatum txOut sd =
    case txOutDatum txOut of
      d@(TxOutSupplementalDatum _) -> addDatum d sd
      _ -> sd

-- | Alter a transaction's redeemers map given some mapping function.
alterRedeemers ::
  ( Ledger.PlutusPurpose Ledger.AsIx LedgerEra ->
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
  ([(TxIn, Maybe HashableScriptData)] -> [(TxIn, Maybe HashableScriptData)]) ->
  Tx ->
  Tx
alterTxIns fn tx =
  Tx body' wits
 where
  body' = ShelleyTxBody ledgerBody' scripts scriptData' mAuxData scriptValidity

  ledgerBody' = ledgerBody & set inputsTxBodyL inputs'

  inputs' = Set.fromList $ toLedgerTxIn . fst <$> newSortedInputs

  scriptData' = TxBodyScriptData dats redeemers'

  redeemers' = Ledger.Redeemers $ rebuiltSpendingRedeemers <> nonSpendingRedeemers

  nonSpendingRedeemers =
    Map.filterWithKey
      ( \x _ -> case x of
          ConwaySpending _ -> False
          _ -> True
      )
      redeemersMap

  rebuiltSpendingRedeemers :: Map.Map (ConwayPlutusPurpose AsIx LedgerEra) (Ledger.Data LedgerEra, Ledger.ExUnits)
  rebuiltSpendingRedeemers = Map.fromList $
    flip mapMaybe (zip [0 ..] newSortedInputs) $ \(i, (_, mRedeemer)) ->
      mRedeemer <&> \d ->
        (ConwaySpending (AsIx i), (toLedgerData d, Ledger.ExUnits 0 0))

  -- NOTE: This needs to be ordered, such that we can calculate the redeemer
  -- pointers correctly.
  newSortedInputs :: [(TxIn, Maybe HashableScriptData)]
  newSortedInputs =
    sortOn fst
      $ fn
        . resolveRedeemers
        . fmap fromLedgerTxIn
        . toList
      $ view inputsTxBodyL ledgerBody

  resolveRedeemers :: [TxIn] -> [(TxIn, Maybe HashableScriptData)]
  resolveRedeemers txInputs =
    zip txInputs [0 ..] <&> \(txIn, i) ->
      case Map.lookup (ConwaySpending (AsIx i)) redeemersMap of
        Nothing -> (txIn, Nothing)
        Just (redeemerData, _exUnits) -> (txIn, Just $ fromLedgerData redeemerData)

  (dats, redeemersMap) = case scriptData of
    TxBodyNoScriptData -> (mempty, mempty)
    TxBodyScriptData d (Ledger.Redeemers r) -> (d, r)

  ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body

  Tx body wits = tx

-- | Apply some mapping function over a transaction's outputs.
alterTxOuts ::
  ([TxOut CtxTx] -> [TxOut CtxTx]) ->
  Tx ->
  Tx
alterTxOuts fn tx =
  Tx body' wits
 where
  body' = ShelleyTxBody ledgerBody' scripts scriptData' mAuxData scriptValidity
  ledgerBody' = ledgerBody & outputsTxBodyL .~ ledgerOutputs'

  ledgerOutputs' = StrictSeq.fromList . map (toLedgerTxOut . toCtxUTxOTxOut) $ outputs'

  outputs' = fn . fmap fromLedgerTxOut . toList $ ledgerBody ^. outputsTxBodyL

  scriptData' = ensureDatums outputs' scriptData

  ShelleyTxBody ledgerBody scripts scriptData mAuxData scriptValidity = body
  Tx body wits = tx

-- | A 'Mutation' that changes the minted/burnt quantity of all tokens to a
-- non-zero value different than the given one.
changeMintedValueQuantityFrom :: Tx -> Integer -> Gen Mutation
changeMintedValueQuantityFrom tx exclude = do
  someQuantity <- fromInteger <$> arbitrary `suchThat` (/= exclude) `suchThat` (/= 0)
  pure $ ChangeMintedValue $ fromList $ map (second $ const someQuantity) $ toList mintedValue
 where
  mintedValue = txMintValueToValue $ txMintValue $ getTxBodyContent $ txBody tx

-- | A 'Mutation' that changes the minted/burned quantity of tokens like this:
-- - when no value is being minted/burned -> add a value
-- - when tx is minting or burning values -> add more values on top of that
changeMintedTokens :: Tx -> Value -> Gen Mutation
changeMintedTokens tx mintValue =
  pure $ ChangeMintedValue $ mintedValue <> mintValue
 where
  mintedValue = txMintValueToValue $ txMintValue $ getTxBodyContent $ txBody tx

-- | A `Mutation` that adds an `Arbitrary` participation token with some quantity.
-- As usual the quantity can be positive for minting, or negative for burning.
addPTWithQuantity :: Tx -> Quantity -> Gen Mutation
addPTWithQuantity tx quantity =
  ChangeMintedValue
    <$>
    -- NOTE: We do not expect Ada or any other assets to be minted, so
    -- we can take the policy id from the head
    case Prelude.head $ toList mintedValue of
      (AdaAssetId, _) -> error "unexpected mint of Ada"
      (AssetId pid _an, _) -> do
        -- Some arbitrary token name, which could correspond to a pub key hash
        pkh <- arbitrary
        pure $ mintedValue <> fromList [(AssetId pid pkh, quantity)]
 where
  mintedValue = txMintValueToValue $ txMintValue $ getTxBodyContent $ txBody tx

-- | Replace first given 'PolicyId' with the second argument in the whole 'TxOut' value.
replacePolicyIdWith :: PolicyId -> PolicyId -> TxOut a -> TxOut a
replacePolicyIdWith original replacement =
  modifyTxOutValue (replacePolicyInValue original replacement)

-- | Replace first given 'PolicyId' with the second argument in the whole 'Value'.
replacePolicyInValue :: PolicyId -> PolicyId -> Value -> Value
replacePolicyInValue original replacement =
  fromList . map replaceAssetId . toList
 where
  replaceAssetId (aid, q) = case aid of
    AssetId pid an
      | pid == original -> (AssetId replacement an, q)
    _ -> (aid, q)

replaceSnapshotVersion :: Head.SnapshotVersion -> Head.State -> Head.State
replaceSnapshotVersion snapshotVersion = \case
  Head.Open Head.OpenDatum{parties, utxoHash, headId, contestationPeriod} ->
    Head.Open
      Head.OpenDatum
        { Head.parties = parties
        , Head.utxoHash = utxoHash
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.version = snapshotVersion
        }
  Head.Closed Head.ClosedDatum{parties, snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, contestationDeadline, headId, contesters, contestationPeriod} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = snapshotVersion
        }
  otherState -> otherState

replaceSnapshotNumber :: Head.SnapshotNumber -> Head.State -> Head.State
replaceSnapshotNumber snapshotNumber = \case
  Head.Closed Head.ClosedDatum{parties, utxoHash, alphaUTxOHash, omegaUTxOHash, contestationDeadline, headId, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

replaceParties :: [Data.Party] -> Head.State -> Head.State
replaceParties parties = \case
  Head.Initial{contestationPeriod, headId, seed} ->
    Head.Initial
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.headId = headId
      , Head.seed = seed
      }
  Head.Open Head.OpenDatum{contestationPeriod, utxoHash, headId, version} ->
    Head.Open
      Head.OpenDatum
        { Head.contestationPeriod = contestationPeriod
        , Head.parties = parties
        , Head.utxoHash = utxoHash
        , Head.headId = headId
        , Head.version = version
        }
  Head.Closed Head.ClosedDatum{snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, contestationDeadline, headId, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

replaceUTxOHash :: Head.Hash -> Head.State -> Head.State
replaceUTxOHash utxoHash = \case
  Head.Open Head.OpenDatum{contestationPeriod, parties, headId, version} ->
    Head.Open
      Head.OpenDatum
        { Head.contestationPeriod = contestationPeriod
        , Head.parties = parties
        , Head.utxoHash = utxoHash
        , Head.headId = headId
        , Head.version = version
        }
  Head.Closed Head.ClosedDatum{parties, alphaUTxOHash, omegaUTxOHash, snapshotNumber, contestationDeadline, headId, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

replaceOmegaUTxOHash :: Head.Hash -> Head.State -> Head.State
replaceOmegaUTxOHash omegaUTxOHash' = \case
  Head.Closed Head.ClosedDatum{parties, utxoHash, alphaUTxOHash, snapshotNumber, contestationDeadline, headId, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash'
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

replaceContestationDeadline :: POSIXTime -> Head.State -> Head.State
replaceContestationDeadline contestationDeadline = \case
  Head.Closed Head.ClosedDatum{snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, parties, headId, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { snapshotNumber
        , utxoHash
        , alphaUTxOHash
        , omegaUTxOHash
        , parties
        , contestationDeadline
        , contestationPeriod
        , headId
        , contesters
        , version
        }
  otherState -> otherState

replaceContestationPeriod :: ContestationPeriod -> Head.State -> Head.State
replaceContestationPeriod contestationPeriod = \case
  Head.Closed Head.ClosedDatum{snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, parties, headId, contesters, contestationDeadline, version} ->
    Head.Closed
      Head.ClosedDatum
        { snapshotNumber
        , utxoHash
        , alphaUTxOHash
        , omegaUTxOHash
        , parties
        , contestationDeadline
        , contestationPeriod
        , headId
        , contesters
        , version
        }
  otherState -> otherState

replaceHeadId :: CurrencySymbol -> Head.State -> Head.State
replaceHeadId headId = \case
  Head.Initial{contestationPeriod, parties, seed} ->
    Head.Initial
      { Head.contestationPeriod = contestationPeriod
      , Head.parties = parties
      , Head.headId = headId
      , Head.seed = seed
      }
  Head.Open Head.OpenDatum{contestationPeriod, utxoHash, parties, version} ->
    Head.Open
      Head.OpenDatum
        { Head.contestationPeriod = contestationPeriod
        , Head.parties = parties
        , Head.utxoHash = utxoHash
        , Head.headId = headId
        , Head.version = version
        }
  Head.Closed Head.ClosedDatum{snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, contestationDeadline, parties, contesters, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

replaceContesters :: [Plutus.PubKeyHash] -> Head.State -> Head.State
replaceContesters contesters = \case
  Head.Closed Head.ClosedDatum{snapshotNumber, utxoHash, alphaUTxOHash, omegaUTxOHash, contestationDeadline, parties, headId, contestationPeriod, version} ->
    Head.Closed
      Head.ClosedDatum
        { Head.parties = parties
        , Head.snapshotNumber = snapshotNumber
        , Head.utxoHash = utxoHash
        , Head.alphaUTxOHash = alphaUTxOHash
        , Head.omegaUTxOHash = omegaUTxOHash
        , Head.contestationDeadline = contestationDeadline
        , Head.contestationPeriod = contestationPeriod
        , Head.headId = headId
        , Head.contesters = contesters
        , Head.version = version
        }
  otherState -> otherState

removePTFromMintedValue :: TxOut CtxUTxO -> Tx -> Value
removePTFromMintedValue output tx =
  case toList $ txMintValueToValue $ txMintValue $ getTxBodyContent $ txBody tx of
    [] -> error "expected minted value"
    v -> fromList $ filter (not . isPT) v
 where
  outValue = txOutValue output
  assetNames =
    [ (policyId, pkh) | (AssetId policyId pkh, _) <- toList outValue, policyId == testPolicyId
    ]
  (headId, assetName) =
    case assetNames of
      [assetId] -> assetId
      _ -> error "expected one assetId"
  isPT = \case
    (AssetId pid asset, _) ->
      pid == headId && asset == assetName
    _ -> False

randomBetween :: Integer -> Integer -> IO Integer
randomBetween min' max' =
  fst . randomR (min', max') <$> newStdGen
