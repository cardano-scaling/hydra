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
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (closeTx, fanoutTx, policyId)
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.Hash as Hash
import Hydra.Contract.MockHead (
  serialiseTxOuts,
  verifyPartySignature,
  verifySnapshotSignature,
 )
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Data.Party (partyFromVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (
  AlonzoEra,
  BuildTxWith (BuildTxWith),
  CardanoTx,
  CtxTx,
  CtxUTxO,
  Era,
  ExecutionUnits (ExecutionUnits),
  LedgerEra,
  PlutusScriptV1,
  Tx (Tx),
  TxBodyScriptData (TxBodyNoScriptData, TxBodyScriptData),
  TxOut (..),
  Utxo,
  Utxo' (Utxo),
  addInputs,
  describeCardanoTx,
  emptyTxBody,
  fromAlonzoExUnits,
  fromLedgerTx,
  fromLedgerTxOut,
  fromLedgerUtxo,
  fromPlutusScript,
  genOutput,
  genUtxoWithoutLegacy,
  genValue,
  getOutputs,
  hashTxOuts,
  lovelaceToTxOutValue,
  mkDatumForTxIn,
  mkRedeemerForTxIn,
  mkScriptAddress,
  mkScriptWitness,
  mkTxOutDatum,
  mkTxOutDatumHash,
  modifyTxOutValue,
  shrinkUtxo,
  toCtxUTxOTxOut,
  toLedgerTxOut,
  txOutValue,
  unsafeBuildTransaction,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (
  MultiSigned (MultiSigned),
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
    prop "OffChain.hashTxOuts == OnChain.hashTxOuts" prop_consistentOnAndOffChainHashOfTxOuts
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

  describe "Hash" $
    it "runs with these ^ execution units over Baseline" $ do
      for_ [0 .. 5] $ \(power :: Integer) -> do
        let n = 8 ^ power
            s = n `quot` 8
        putTextLn @IO $ "    n = " <> show n <> ", s = " <> show s
        for_ [minBound .. maxBound] $ \algorithm -> do
          let ExecutionUnits
                { executionSteps = baseCpu
                , executionMemory = baseMem
                } = calculateHashExUnits n Hash.Base
              units@ExecutionUnits
                { executionSteps = cpu
                , executionMemory = mem
                } = calculateHashExUnits n algorithm
          putTextLn $
            "      " <> show algorithm
              <> ": "
              <> show units
              <> " Δcpu="
              <> show (toInteger cpu - toInteger baseCpu)
              <> " Δmem="
              <> show (toInteger mem - toInteger baseMem)

calculateHashExUnits :: Int -> Hash.HashAlgorithm -> ExecutionUnits
calculateHashExUnits n algorithm =
  case evaluateTx tx utxo of
    Left basicFailure ->
      error ("Basic failure: " <> show basicFailure)
    Right report ->
      case Map.elems report of
        [Right units] ->
          fromAlonzoExUnits units
        _ ->
          error $ "Too many redeemers in report: " <> show report
 where
  tx = unsafeBuildTransaction $ emptyTxBody & addInputs [(input, witness)]
  utxo = Utxo $ Map.singleton input output
  input = generateWith arbitrary 42
  output = toCtxUTxOTxOut $ TxOut address value (mkTxOutDatum datum)
  value = lovelaceToTxOutValue 1_000_000
  address = mkScriptAddress @PlutusScriptV1 testNetworkId script
  witness = BuildTxWith $ mkScriptWitness script (mkDatumForTxIn datum) redeemer
  script = fromPlutusScript Hash.validatorScript
  datum = Hash.datum $ toBuiltin bytes
  redeemer = mkRedeemerForTxIn $ Hash.redeemer algorithm
  bytes = fold $ replicate n ("0" :: ByteString)

--
-- Properties
--

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUtxoWithoutLegacy shrinkUtxo $ \(utxo :: Utxo) ->
    let plutusTxOuts = mapMaybe (txInfoOut . toLedgerTxOut) ledgerTxOuts
        ledgerTxOuts = toList utxo
        plutusBytes = serialiseTxOuts plutusTxOuts
        ledgerBytes = serialize' (toLedgerTxOut <$> ledgerTxOuts)
     in (hashTxOuts ledgerTxOuts === fromBuiltin (MockHead.hashTxOuts plutusTxOuts))
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
  = ChangeHeadRedeemer MockHead.Input
  | ChangeHeadDatum MockHead.State
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

healthyCloseDatum :: MockHead.State
healthyCloseDatum =
  MockHead.Open
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
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (/= healthySnapshotNumber)
        pure (closeRedeemer mutatedSnapshotNumber $ healthySignature healthySnapshotNumber)
    , SomeMutation MutateSnapshotToIllFormedValue . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              MultiSigned [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthyPartyCredentials]
        pure
          MockHead.Close
            { snapshotNumber = mutatedSnapshotNumber
            , signature = toPlutusSignatures mutatedSignature
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyCloseParties)
        pure $
          MockHead.Open
            { parties = mutatedParties
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> arbitrary `suchThat` \case
        MockHead.Open{MockHead.parties = parties} ->
          parties /= MockHead.parties healthyCloseDatum
        _ ->
          True
    ]
 where
  closeRedeemer snapshotNumber sig =
    MockHead.Close
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
  generateWith genUtxoWithoutLegacy 42

healthyFanoutDatum :: MockHead.State
healthyFanoutDatum =
  MockHead.Closed 1 (toBuiltin $ hashTxOuts $ toList healthyFanoutUtxo)

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

deriving instance Eq MockHead.Input

instance Arbitrary MockHead.Input where
  arbitrary = genericArbitrary

instance Arbitrary MockHead.State where
  arbitrary = genericArbitrary

--
-- Helpers
--

isHeadOutput :: TxOut CtxUTxO Era -> Bool
isHeadOutput (TxOut addr _ _) = addr == headAddress
 where
  headAddress = Api.mkScriptAddress @Api.PlutusScriptV1 Fixture.testNetworkId headScript
  headScript = Api.fromPlutusScript $ MockHead.validatorScript policyId

changeHeadRedeemer :: MockHead.Input -> (Ledger.Data era, Ledger.ExUnits) -> (Ledger.Data era, Ledger.ExUnits)
changeHeadRedeemer newRedeemer redeemer@(dat, units) =
  case fromData (Ledger.getPlutusData dat) of
    Just (_ :: MockHead.Input) ->
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
