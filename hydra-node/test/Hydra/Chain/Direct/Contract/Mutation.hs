{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provides building blocks for Mutation testing of Contracts.
module Hydra.Chain.Direct.Contract.Mutation where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.Shelley (TxBody (ShelleyTxBody))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (
  policyId,
 )
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (
  AlonzoEra,
  CardanoTx,
  CtxTx,
  CtxUTxO,
  Era,
  LedgerEra,
  Tx (Tx),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  TxIn,
  TxOut (..),
  TxOutDatum (..),
  Utxo,
  describeCardanoTx,
  fromLedgerTxOut,
  mkTxOutDatum,
  mkTxOutDatumHash,
  toAlonzoData,
  toCtxUTxOTxOut,
  toLedgerTxOut,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (
  SigningKey,
  generateKey,
 )
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toData)
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
propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & genericCoverTable [label]
      & checkCoverage

-- | A 'Property' checking some (transaction, UTxO) pair is invalid.
propTransactionDoesNotValidate :: (CardanoTx, Utxo) -> Property
propTransactionDoesNotValidate (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in case result of
        Left _ ->
          property True
        Right redeemerReport ->
          any isLeft (Map.elems redeemerReport)
            & counterexample ("Tx: " <> toString (describeCardanoTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation should have failed"

-- | A 'Property' checking some (transaction, UTxO) pair is valid.
propTransactionValidates :: (CardanoTx, Utxo) -> Property
propTransactionValidates (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in case result of
        Left basicFailure ->
          property False
            & counterexample ("Tx: " <> toString (describeCardanoTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
            & counterexample ("Phase-1 validation failed: " <> show basicFailure)
        Right redeemerReport ->
          all isRight (Map.elems redeemerReport)
            & counterexample ("Tx: " <> toString (describeCardanoTx tx))
            & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
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
    PrependOutput (TxOut CtxTx Era)
  | -- | Change an input's 'TxOut' to something else.
    -- This mutation alters the redeemers of the transaction to ensure
    -- any matching redeemer for given input is removed, otherwise the
    -- transaction would be invalid for the wrong reason (unused redeemer).
    --
    -- NOTE: The changed output should not be spending a script address as
    -- we don't provide any redeemer for it.
    ChangeInput TxIn (TxOut CtxUTxO Era)
  | -- | Change the transaction's output at given index to something else.
    ChangeOutput Word (TxOut CtxTx Era)
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
applyMutation :: Mutation -> (CardanoTx, Utxo) -> (CardanoTx, Utxo)
applyMutation mutation (tx@(Tx body wits), utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    let ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
        headOutputIndices =
          fst
            <$> filter
              (isHeadOutput . snd . snd)
              (zip [0 :: Word64 ..] $ Map.toAscList $ Api.utxoMap utxo)
        headInputIdx = case headOutputIndices of
          [i] -> i
          _ -> error $ "could not find head output in utxo: " <> show utxo

        newHeadRedeemer (Ledger.RdmrPtr _ ix) (dat, units)
          | ix == headInputIdx = (Ledger.Data (toData newRedeemer), units)
          | otherwise = (dat, units)

        redeemers = alterRedeemers newHeadRedeemer scriptData
        body' = ShelleyTxBody era ledgerBody scripts redeemers mAuxData scriptValidity
     in (Tx body' wits, utxo)
  ChangeHeadDatum d' ->
    let datum = mkTxOutDatum d'
        datumHash = mkTxOutDatumHash d'
        -- change the lookup UTXO
        fn o@(TxOut addr value _)
          | isHeadOutput o =
            TxOut addr value datumHash
          | otherwise =
            o
        -- change the datums in the tx
        ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
        newDatums = addDatum datum scriptData
        body' = ShelleyTxBody era ledgerBody scripts newDatums mAuxData scriptValidity
     in (Tx body' wits, fmap fn utxo)
  PrependOutput txOut ->
    ( alterTxOuts (txOut :) tx
    , utxo
    )
  ChangeInput txIn txOut ->
    ( Tx body' wits
    , Api.Utxo $ Map.insert txIn txOut (Api.utxoMap utxo)
    )
   where
    ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
    redeemers = removeRedeemerFor (Ledger.inputs ledgerBody) (Api.toLedgerTxIn txIn) scriptData
    body' = ShelleyTxBody era ledgerBody scripts redeemers mAuxData scriptValidity
  ChangeOutput ix txOut ->
    ( alterTxOuts replaceAtIndex tx
    , utxo
    )
   where
    replaceAtIndex txOuts =
      foldr
        ( \(i, out) list ->
            if i == ix then txOut : list else out : list
        )
        []
        (zip [0 ..] txOuts)
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
isHeadOutput :: TxOut CtxUTxO Era -> Bool
isHeadOutput (TxOut addr _ _) = addr == headAddress
 where
  headAddress = Api.mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId headScript
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

isInitialOutput :: TxOut CtxUTxO Era -> Bool
isInitialOutput (TxOut addr _ _) = addr == initialAddress
 where
  initialAddress = Api.mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId initialScript
  initialScript = Api.fromPlutusScript Initial.validatorScript

-- | Adds given 'Datum' and corresponding hash to the transaction's scripts.
-- TODO: As we are creating the `TxOutDatum` from a known datum, passing a `TxOutDatum` is
-- pointless and requires more work than needed to check impossible variants.
addDatum :: TxOutDatum CtxTx Era -> TxBodyScriptData Era -> TxBodyScriptData Era
addDatum datum txBodyScriptData =
  case datum of
    TxOutDatumNone -> error "unexpected datuym none"
    TxOutDatumHash _sdsie _ha -> error "hash only, expected full datum"
    TxOutDatum _ha sd ->
      case txBodyScriptData of
        TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
        TxBodyScriptData supportedInEra (Ledger.TxDats dats) redeemers ->
          let dat = toAlonzoData sd
              newDats = Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats
           in TxBodyScriptData supportedInEra newDats redeemers

-- | Alter a transaction's  redeemers map given some mapping function.
alterRedeemers ::
  ( Ledger.RdmrPtr ->
    (Ledger.Data LedgerEra, Ledger.ExUnits) ->
    (Ledger.Data LedgerEra, Ledger.ExUnits)
  ) ->
  TxBodyScriptData AlonzoEra ->
  TxBodyScriptData AlonzoEra
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers redeemers) ->
    let newRedeemers = Map.mapWithKey fn redeemers
     in TxBodyScriptData supportedInEra dats (Ledger.Redeemers newRedeemers)

-- | Remove redeemer for given `TxIn` from the transaction's redeemers map.
removeRedeemerFor ::
  Set (Ledger.TxIn a) ->
  Ledger.TxIn a ->
  TxBodyScriptData AlonzoEra ->
  TxBodyScriptData AlonzoEra
removeRedeemerFor initialInputs txIn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers initialRedeemers) ->
    let newRedeemers = Ledger.Redeemers $ Map.fromList $ filter removeRedeemer $ Map.toList initialRedeemers
        sortedInputs = sort $ toList initialInputs
        removeRedeemer (Ledger.RdmrPtr _ idx, _) = sortedInputs List.!! fromIntegral idx /= txIn
     in TxBodyScriptData supportedInEra dats newRedeemers

-- | Apply some mapping function over a transaction's  outputs.
alterTxOuts ::
  ([TxOut CtxTx Era] -> [TxOut CtxTx Era]) ->
  CardanoTx ->
  CardanoTx
alterTxOuts fn tx =
  Tx body' wits
 where
  body' = ShelleyTxBody era ledgerBody' scripts scriptData mAuxData scriptValidity
  ledgerBody' = ledgerBody{Ledger.outputs = outputs'}
  -- WIP
  outputs' = StrictSeq.fromList . mapOutputs . toList $ Ledger.outputs ledgerBody
  mapOutputs = fmap (toLedgerTxOut . toCtxUTxOTxOut) . fn . fmap fromLedgerTxOut
  ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
  Tx body wits = tx
