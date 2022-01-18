{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.Shelley (TxBody (ShelleyTxBody))
import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import Cardano.Ledger.Alonzo.TxInfo (txInfoOut)
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (
  closeTx,
  collectComTx,
  fanoutTx,
  mkCommitDatum,
  policyId,
 )
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (serialiseTxOuts)
import Hydra.Contract.Head (
  verifyPartySignature,
  verifySnapshotSignature,
 )
import qualified Hydra.Contract.Head as Head
import Hydra.Data.Party (partyFromVerKey)
import qualified Hydra.Data.Party as OnChain
import qualified Hydra.Data.Party as Party
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
  Utxo,
  adaOnly,
  describeCardanoTx,
  fromLedgerTx,
  fromLedgerTxOut,
  fromLedgerUtxo,
  fromPlutusScript,
  genAdaOnlyUtxo,
  genOutput,
  genUtxoWithSimplifiedAddresses,
  genValue,
  getOutputs,
  hashTxOuts,
  lovelaceToValue,
  mkScriptAddress,
  mkTxOutDatum,
  mkTxOutDatumHash,
  mkTxOutValue,
  modifyTxOutValue,
  shrinkUtxo,
  toCtxUTxOTxOut,
  toLedgerTxIn,
  toLedgerTxOut,
  txOutValue,
  utxoPairs,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (
  MultiSigned (MultiSigned),
  Party,
  Signed (UnsafeSigned),
  SigningKey,
  aggregate,
  deriveParty,
  generateKey,
  sign,
  toPlutusSignatures,
  vkey,
 )
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromBuiltin, fromData, toBuiltin, toData)
import Plutus.V1.Ledger.Crypto (Signature (Signature))
import Test.QuickCheck (
  Positive (Positive),
  Property,
  arbitrarySizedNatural,
  checkCoverage,
  choose,
  counterexample,
  elements,
  forAll,
  forAllShrink,
  oneof,
  property,
  suchThat,
  (===),
 )
import Test.QuickCheck.Instances ()
import qualified Prelude

spec :: Spec
spec = do
  describe "Signature validator" $ do
    prop
      "verifies single signature produced off-chain"
      prop_verifyOffChainSignatures
    -- FIXME(AB): This property exists solely because our current multisignature implementation
    -- is just the aggregates of individual (mock) signatures and there is no point in doing some
    -- complicated shuffle logic to verify signatures given we'll end up verifying a single Ed25519
    -- signatures.
    prop
      "verifies snapshot multi-signature for list of parties and signatures"
      prop_verifySnapshotSignatures
  describe "TxOut hashing" $ do
    modifyMaxSuccess (const 20) $
      prop "OffChain.hashTxOuts == OnChain.hashTxOuts" prop_consistentOnAndOffChainHashOfTxOuts
  describe "CollectCom" $ do
    prop "is healthy" $
      propTransactionValidates healthyCollectComTx
  describe "Close" $ do
    prop "is healthy" $
      propTransactionValidates healthyCloseTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseTx genCloseMutation
  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionValidates healthyFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyFanoutTx genFanoutMutation

--
-- Properties
--

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUtxoWithSimplifiedAddresses shrinkUtxo $ \(utxo :: Utxo) ->
    let plutusTxOuts = mapMaybe (txInfoOut . toLedgerTxOut) ledgerTxOuts
        ledgerTxOuts = toList utxo
        plutusBytes = serialiseTxOuts plutusTxOuts
        ledgerBytes = serialize' (toLedgerTxOut <$> ledgerTxOuts)
     in (hashTxOuts ledgerTxOuts === fromBuiltin (Head.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show ledgerTxOuts)
          & counterexample ("Ledger CBOR: " <> decodeUtf8 (Base16.encode ledgerBytes))
          & counterexample ("Plutus CBOR: " <> decodeUtf8 (Base16.encode $ fromBuiltin plutusBytes))

prop_verifyOffChainSignatures :: Property
prop_verifyOffChainSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll arbitrary $ \(Positive n) ->
      let sk = generateKey n
          UnsafeSigned signature = sign sk snapshot
          party = partyFromVerKey $ deriveVerKeyDSIGN sk
          snapshotNumber = toInteger $ number snapshot
       in verifyPartySignature snapshotNumber party (Signature $ toBuiltin signature)
            & counterexample ("signed: " <> show (BS.unpack signature))
            & counterexample ("party: " <> show party)
            & counterexample ("message: " <> show (getSignableRepresentation snapshot))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll genListOfSigningKeys $ \sks ->
      let parties = partyFromVerKey . deriveVerKeyDSIGN <$> sks
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger $ number snapshot
       in verifySnapshotSignature parties snapshotNumber signatures

propMutation :: (CardanoTx, Utxo) -> ((CardanoTx, Utxo) -> Gen SomeMutation) -> Property
propMutation (tx, utxo) genMutation =
  forAll @_ @Property (genMutation (tx, utxo)) $ \SomeMutation{label, mutation} ->
    (tx, utxo)
      & applyMutation mutation
      & propTransactionDoesNotValidate
      & genericCoverTable [label]
      & checkCoverage

propTransactionDoesNotValidate :: (CardanoTx, Utxo) -> Property
propTransactionDoesNotValidate (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in counterexample "Should have not validated" $
        case result of
          Left _ ->
            property True
          Right redeemerReport ->
            any isLeft (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

propTransactionValidates :: (CardanoTx, Utxo) -> Property
propTransactionValidates (tx, lookupUtxo) =
  let result = evaluateTx tx lookupUtxo
   in counterexample "Should have validated" $
        case result of
          Left _ ->
            property False
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
          Right redeemerReport ->
            all isRight (Map.elems redeemerReport)
              & counterexample ("Tx: " <> toString (describeCardanoTx tx))
              & counterexample ("Lookup utxo: " <> decodeUtf8 (encodePretty lookupUtxo))
              & counterexample ("Redeemer report: " <> show redeemerReport)

--
-- Mutation
--

data SomeMutation = forall lbl.
  (Typeable lbl, Enum lbl, Bounded lbl, Show lbl) =>
  SomeMutation
  { label :: lbl
  , mutation :: Mutation
  }
deriving instance Show SomeMutation

data Mutation
  = ChangeHeadRedeemer Head.Input
  | ChangeHeadDatum Head.State
  | PrependOutput (TxOut CtxTx Era)
  | ChangeOutput Word (TxOut CtxTx Era)
  deriving (Show, Generic)

applyMutation :: Mutation -> (CardanoTx, Utxo) -> (CardanoTx, Utxo)
applyMutation mutation (tx@(Tx body wits), utxo) = case mutation of
  ChangeHeadRedeemer newRedeemer ->
    let ShelleyTxBody era ledgerBody scripts scriptData mAuxData scriptValidity = body
        redeemers = alterRedeemers (changeHeadRedeemer newRedeemer) scriptData
        body' = ShelleyTxBody era ledgerBody scripts redeemers mAuxData scriptValidity
     in (Tx body' wits, utxo)
  ChangeHeadDatum d' ->
    let fn o@(TxOut addr value _)
          | isHeadOutput o =
            (TxOut addr value $ mkTxOutDatumHash d')
          | otherwise =
            o
     in (tx, fmap fn utxo)
  PrependOutput txOut ->
    ( alterTxOuts (txOut :) tx
    , utxo
    )
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

--
-- CollectComTx
--

healthyCollectComTx :: (CardanoTx, Utxo)
healthyCollectComTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  lookupUtxo =
    Ledger.UTxO $
      Map.singleton headInput headResolvedInput <> (fst <$> commits)

  tx =
    collectComTx
      Fixture.testNetworkId
      (Api.Utxo $ Map.fromList committedUtxo)
      (headInput, headDatum, healthyCollectComOnChainParties)
      commits

  committedUtxo =
    generateWith
      (replicateM (length healthyCollectComParties) genCommittableTxOut)
      42

  commits =
    (uncurry healthyCommitOutput <$> zip healthyCollectComParties committedUtxo)
      & Map.fromList
      & Map.mapKeys toLedgerTxIn
      & Map.map (first toLedgerTxOut)

  headInput = generateWith arbitrary 42
  headResolvedInput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCollectComDatum

healthyCollectComDatum :: Head.State
healthyCollectComDatum =
  Head.Initial
    { contestationPeriod = generateWith arbitrary 42
    , parties = healthyCollectComOnChainParties
    }

healthyCollectComOnChainParties :: [OnChain.Party]
healthyCollectComOnChainParties =
  Party.partyFromVerKey . vkey <$> healthyCollectComParties

healthyCollectComParties :: [Party]
healthyCollectComParties = flip generateWith 42 $ do
  alice <- arbitrary
  bob <- arbitrary
  carol <- arbitrary
  pure [alice, bob, carol]

genCommittableTxOut :: Gen (TxIn, TxOut CtxUTxO AlonzoEra)
genCommittableTxOut =
  Prelude.head . utxoPairs <$> (genAdaOnlyUtxo `suchThat` (\u -> length u > 1))

healthyCommitOutput ::
  Party ->
  (TxIn, TxOut CtxUTxO AlonzoEra) ->
  (TxIn, (TxOut CtxUTxO AlonzoEra, Ledger.Data LedgerEra))
healthyCommitOutput party committed =
  ( generateWith arbitrary seed
  ,
    ( toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatum commitDatum))
    , Ledger.Data (toData commitDatum)
    )
  )
 where
  Party.UnsafeParty (fromIntegral -> seed) = Party.partyFromVerKey (vkey party)

  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId commitScript
  commitValue =
    mkTxOutValue $
      lovelaceToValue 2_000_000 <> (txOutValue . snd) committed
  commitDatum =
    mkCommitDatum party (Just committed)

--
-- CloseTx
--

healthyCloseTx :: (CardanoTx, Utxo)
healthyCloseTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = closeTx healthySnapshot (healthySignature healthySnapshotNumber) (headInput, headOutput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCloseDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthySnapshot :: Snapshot CardanoTx
healthySnapshot =
  Snapshot
    { number = healthySnapshotNumber
    , utxo = mempty
    , confirmed = []
    }

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthyCloseDatum :: Head.State
healthyCloseDatum =
  Head.Open
    { parties = healthyCloseParties
    , utxoHash = ""
    }

healthyCloseParties :: [OnChain.Party]
healthyCloseParties = partyFromVerKey . vkey . deriveParty <$> healthyPartyCredentials

healthyPartyCredentials :: [SigningKey]
healthyPartyCredentials = [1, 2, 3]

healthySignature :: SnapshotNumber -> MultiSigned (Snapshot CardanoTx)
healthySignature number = MultiSigned [sign sk snapshot | sk <- healthyPartyCredentials]
 where
  snapshot = healthySnapshot{number}

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotToIllFormedValue
  | MutateParties
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genCloseMutation (_tx, _utxo) =
  -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
  -- the power of the mutators, we should test at the level of the validator.
  -- That is, using the on-chain types. 'closeRedeemer' is also not used
  -- anywhere after changing this and can be moved into the closeTx
  oneof
    [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        closeRedeemer (number healthySnapshot) <$> arbitrary
    , SomeMutation MutateSnapshotNumberButNotSignature . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (\n -> n /= healthySnapshotNumber && n > 0)
        pure (closeRedeemer mutatedSnapshotNumber $ healthySignature healthySnapshotNumber)
    , SomeMutation MutateSnapshotToIllFormedValue . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              MultiSigned [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthyPartyCredentials]
        pure
          Head.Close
            { snapshotNumber = mutatedSnapshotNumber
            , signature = toPlutusSignatures mutatedSignature
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyCloseParties)
        pure $
          Head.Open
            { parties = mutatedParties
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> arbitrary `suchThat` \case
        Head.Open{Head.parties = parties} ->
          parties /= Head.parties healthyCloseDatum
        _ ->
          True
    ]
 where
  closeRedeemer snapshotNumber sig =
    Head.Close
      { snapshotNumber = toInteger snapshotNumber
      , signature = toPlutusSignatures sig
      , utxoHash = ""
      }

--
-- FanoutTx
--

healthyFanoutTx :: (CardanoTx, Utxo)
healthyFanoutTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = fanoutTx healthyFanoutUtxo (headInput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyFanoutDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthyFanoutUtxo :: Utxo
healthyFanoutUtxo =
  -- NOTE: we trim down the generated tx's output to make sure it fits w/in
  -- TX size limits
  adaOnly <$> generateWith genUtxoWithSimplifiedAddresses 42

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed 1 (toBuiltin $ hashTxOuts $ toList healthyFanoutUtxo)

data FanoutMutation
  = MutateAddUnexpectedOutput
  | MutateChangeOutputValue
  deriving (Generic, Show, Enum, Bounded)

genFanoutMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutput
    , SomeMutation MutateChangeOutputValue <$> do
        let outs = getOutputs tx
        (ix, out) <- elements (zip [0 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    ]

--
-- Generators
--

genListOfSigningKeys :: Gen [SigningKey]
genListOfSigningKeys = choose (1, 20) <&> fmap generateKey . enumFromTo 1

genBytes :: Gen ByteString
genBytes = arbitrary

---
--- Orphans
---

deriving instance Eq Head.Input

instance Arbitrary Head.Input where
  arbitrary = genericArbitrary

instance Arbitrary Head.State where
  arbitrary = genericArbitrary

--
-- Helpers
--

isHeadOutput :: TxOut CtxUTxO Era -> Bool
isHeadOutput (TxOut addr _ _) = addr == headAddress
 where
  headAddress = Api.mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId headScript
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

changeHeadRedeemer :: Head.Input -> (Ledger.Data era, Ledger.ExUnits) -> (Ledger.Data era, Ledger.ExUnits)
changeHeadRedeemer newRedeemer redeemer@(dat, units) =
  case fromData (Ledger.getPlutusData dat) of
    Just (_ :: Head.Input) ->
      (Ledger.Data (toData newRedeemer), units)
    Nothing ->
      redeemer

alterRedeemers ::
  ((Ledger.Data LedgerEra, Ledger.ExUnits) -> (Ledger.Data LedgerEra, Ledger.ExUnits)) ->
  TxBodyScriptData AlonzoEra ->
  TxBodyScriptData AlonzoEra
alterRedeemers fn = \case
  TxBodyNoScriptData -> error "TxBodyNoScriptData unexpected"
  TxBodyScriptData supportedInEra dats (Ledger.Redeemers redeemers) ->
    let newRedeemers = fmap fn redeemers
     in TxBodyScriptData supportedInEra dats (Ledger.Redeemers newRedeemers)

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
