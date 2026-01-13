{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Tx.Gen where

import Hydra.Cardano.Api hiding (generateSigningKey)
import Hydra.Prelude hiding (toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.DSIGN qualified as CC
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Api (ensureMinCoinTxOut, setMinCoinTxOut)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Codec.CBOR.Magic (uintegerFromBytes)
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.ChainState
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Head qualified as Head
import Hydra.Ledger.Cardano.Evaluate
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Plutus.Orphans ()
import Hydra.Tx
import Hydra.Tx.Accumulator (createCRSG1Datum, defaultItems)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (CloseObservation)
import Hydra.Tx.CollectCom
import Hydra.Tx.ContestationPeriod
import Hydra.Tx.Crypto
import Hydra.Tx.Observe (AbortObservation, CommitObservation, ContestObservation, DecrementObservation, DepositObservation, FanoutObservation, HeadObservation, IncrementObservation, InitObservation, RecoverObservation)
import Hydra.Tx.OnChainId
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Property, choose, counterexample, frequency, listOf, listOf1, oneof, property, scale, shrinkList, shrinkMapBy, sized, suchThat, vector, vectorOf)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary (..))
import Test.QuickCheck.Gen (chooseWord64)

-- * TxOut

instance Arbitrary (TxOut CtxUTxO) where
  arbitrary = genTxOut
  shrink txOut = fromLedgerTxOut <$> shrink (toLedgerTxOut txOut)

-- | Generate a 'Conway' era 'TxOut', which may contain arbitrary assets
-- addressed to public keys and scripts, as well as datums.
--
-- NOTE: This generator does
--  * not produce byron addresses as most of the cardano ecosystem dropped support for that (including plutus),
--  * not produce reference scripts as they are not fully "visible" from plutus,
--  * replace stake pointers with null references as nobody uses that.
genTxOut :: Gen (TxOut ctx)
genTxOut =
  (gen `suchThat` notByronAddress)
    >>= realisticAda
    <&> ensureSomeAda . noRefScripts . noStakeRefPtr
 where
  gen :: Gen (TxOut ctx)
  gen =
    oneof
      [ fromLedgerTxOut <$> arbitrary
      , noDatum . notMultiAsset . fromLedgerTxOut <$> arbitrary
      ]

notMultiAsset :: TxOut ctx -> TxOut ctx
notMultiAsset =
  modifyTxOutValue (lovelaceToValue . selectLovelace)

notByronAddress :: TxOut ctx -> Bool
notByronAddress (TxOut addr _ _ _) = case addr of
  ByronAddressInEra{} -> False
  _ -> True

realisticAda :: TxOut ctx -> Gen (TxOut ctx)
realisticAda o = sized $ \n -> do
  let maxSupply = 45_000_000_000_000_000
      realistic = Coin $ maxSupply `div` fromIntegral (max n 1)
      makeRealistic v =
        let MaryValue c ma = toLedgerValue v
         in fromLedgerValue (MaryValue (min c realistic) ma)
  pure $
    modifyTxOutValue makeRealistic o

ensureSomeAda :: TxOut CtxUTxO -> TxOut ctx
ensureSomeAda =
  fromLedgerTxOut . ensureMinCoinTxOut pparams . toLedgerTxOut

ensureMinAda :: TxOut CtxUTxO -> TxOut ctx
ensureMinAda =
  fromLedgerTxOut . setMinCoinTxOut pparams . toLedgerTxOut

noStakeRefPtr :: TxOut ctx -> TxOut ctx
noStakeRefPtr out@(TxOut addr val dat refScript) = case addr of
  ShelleyAddressInEra (ShelleyAddress _ cre sr) ->
    case sr of
      Ledger.StakeRefPtr _ ->
        TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat refScript
      _ ->
        TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre sr)) val dat refScript
  _ -> out

noRefScripts :: TxOut ctx -> TxOut ctx
noRefScripts out =
  out{txOutReferenceScript = ReferenceScriptNone}

noDatum :: TxOut ctx -> TxOut ctx
noDatum out =
  out{txOutDatum = TxOutDatumNone}

-- | Adjusts a transaction output to remove any policy asset groups that contain
-- non-positive quantities, while preserving the ADA amount. If a 'PolicyId' is
-- provided, all remaining non-ADA assets are collapsed under that single policy.
-- This ensures the output value has no negative or zero asset quantities.
noNegativeAssetsWithPotentialPolicy :: Maybe PolicyId -> TxOut ctx -> TxOut ctx
noNegativeAssetsWithPotentialPolicy mpid out =
  let val = txOutValue out
      nonAdaAssets =
        Map.foldrWithKey
          (\pid policyAssets def -> policyAssetsToValue (fromMaybe pid mpid) policyAssets <> def)
          mempty
          (filterNegativeVals (valueToPolicyAssets val))
      ada = selectLovelace val
   in out{txOutValue = lovelaceToValue ada <> nonAdaAssets}
 where
  filterNegativeVals =
    Map.filterWithKey
      ( \pid passets ->
          all
            (\(_, Quantity n) -> n > 0)
            (toList $ policyAssetsToValue pid passets)
      )

genTxOutWithAssets :: Maybe PolicyId -> Gen (TxOut ctx)
genTxOutWithAssets pid =
  ((fromLedgerTxOut <$> arbitrary) `suchThat` notByronAddress)
    >>= realisticAda
    <&> noNegativeAssetsWithPotentialPolicy pid . ensureMinAda . noRefScripts . noStakeRefPtr

-- | Generate a 'TxOut' with a byron address. This is usually not supported by
-- Hydra or Plutus.
genTxOutByron :: Gen (TxOut ctx)
genTxOutByron = do
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ TxOut addr value TxOutDatumNone ReferenceScriptNone

-- | Generate an ada-only 'TxOut' paid to an arbitrary public key.
genTxOutAdaOnly :: VerificationKey PaymentKey -> Gen (TxOut ctx)
genTxOutAdaOnly vk = do
  value <- lovelaceToValue . Coin <$> scale (* 8) arbitrary `suchThat` (> 0)
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone ReferenceScriptNone

-- | Generate a 'TxOut' with a reference script. The standard 'genTxOut' is not
-- including reference scripts, use this generator if you are interested in
-- these cases.
genTxOutWithReferenceScript :: Gen (TxOut ctx)
genTxOutWithReferenceScript = do
  -- Have the ledger generate a TxOut with a reference script as instances are
  -- not so easily accessible.
  refScript <- (txOutReferenceScript . fromLedgerTxOut <$> arbitrary) `suchThat` (/= ReferenceScriptNone)
  genTxOut <&> \out -> out{txOutReferenceScript = refScript}

-- * UTxO

instance Arbitrary UTxO where
  shrink = shrinkUTxO
  arbitrary = genUTxO

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO = shrinkMapBy (UTxO . fromList) UTxO.toList (shrinkList shrinkOne)
 where
  shrinkOne :: (TxIn, TxOut CtxUTxO) -> [(TxIn, TxOut CtxUTxO)]
  shrinkOne (i, o) = case o of
    TxOut addr value datum refScript ->
      [ (i, TxOut addr value' datum refScript)
      | value' <- shrinkValue value
      ]

-- | Generate a 'Conway' era 'UTxO'. See also 'genTxOut'.
genUTxO :: Gen UTxO
genUTxO = sized genUTxOSized

-- | Generate UTxO with some assets. If `PolicyId` argument is specified then
-- we set assets with the specified 'PolicyId' since in some tests we need to
-- make sure ledger rules are passing.
genUTxOWithAssetsSized :: Int -> Maybe PolicyId -> Gen UTxO
genUTxOWithAssetsSized numUTxO pid =
  fold <$> vectorOf numUTxO gen
 where
  gen :: Gen UTxO
  gen = UTxO.singleton <$> arbitrary <*> genTxOutWithAssets pid

-- | Generate a 'Conway' era 'UTxO' with given number of outputs. See also
-- 'genTxOut'.
genUTxOSized :: Int -> Gen UTxO
genUTxOSized numUTxO =
  fold <$> vectorOf numUTxO gen
 where
  gen :: Gen UTxO
  gen = UTxO.singleton <$> arbitrary <*> genTxOut

-- | Generate a 'UTxO' with a single entry using given 'TxOut' generator.
genUTxO1 :: Gen (TxOut CtxUTxO) -> Gen UTxO
genUTxO1 gen = UTxO.singleton <$> arbitrary <*> gen

-- | Generate utxos owned by the given cardano key.
genUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genUTxOFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inps <- vectorOf n arbitrary
  outs <- vectorOf n (genOutputFor vk)
  pure $ UTxO $ Map.fromList $ zip inps outs

-- | Generate a fixed size UTxO with ada-only outputs.
genUTxOAdaOnlyOfSize :: Int -> Gen UTxO
genUTxOAdaOnlyOfSize numUTxO =
  fold <$> vectorOf numUTxO gen
 where
  gen :: Gen UTxO
  gen = UTxO.singleton <$> arbitrary <*> (genTxOutAdaOnly =<< arbitrary)

-- | Generate a single UTXO owned by 'vk'.
genOneUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genOneUTxOFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutputFor vk
  pure $ UTxO $ Map.singleton input output

-- | Generate "simplified" UTXO, ie. without some of the complexities required
-- for backward-compatibility and obscure features.
genUTxOWithSimplifiedAddresses :: Gen UTxO
genUTxOWithSimplifiedAddresses =
  UTxO.fromList <$> listOf genEntry
 where
  genEntry :: Gen (TxIn, TxOut ctx)
  genEntry = (,) <$> genTxIn <*> genTxOut

-- * Others

instance Arbitrary AssetName where
  arbitrary = UnsafeAssetName . BS.take 32 <$> arbitrary

instance Arbitrary PolicyAssets where
  arbitrary = PolicyAssets <$> arbitrary

instance Arbitrary Quantity where
  arbitrary = Quantity <$> arbitrary

genKeyPair :: Gen (VerificationKey PaymentKey, SigningKey PaymentKey)
genKeyPair = do
  sk <- genSigningKey
  pure (getVerificationKey sk, sk)

genValue :: Gen Value
genValue = fmap ((lovelaceToValue $ Coin 20_000_000) <>) (scale (`div` 10) $ fromLedgerValue <$> arbitrary)

genVerificationKey :: Gen (VerificationKey PaymentKey)
genVerificationKey = getVerificationKey <$> genSigningKey

-- NOTE: This instance is here just because we use this type in Hydra api server so it is ok
-- to generate various network ids.
instance Arbitrary AddressInEra where
  arbitrary = genAddressInEra =<< arbitrary

-- | NOTE: See note on 'mkVkAddress' about 'NetworkId'.
genAddressInEra :: NetworkId -> Gen AddressInEra
genAddressInEra networkId =
  mkVkAddress networkId <$> genVerificationKey

genScriptData :: Gen ScriptData
genScriptData = oneof [ScriptDataBytes <$> arbitrary, ScriptDataNumber <$> arbitrary]

genDatum :: Gen (TxOutDatum ctx)
genDatum = do
  scriptData <-
    oneof
      [ genScriptData
      , ScriptDataList <$> listOf1 genScriptData
      , ScriptDataMap <$> listOf1 ((,) <$> genScriptData <*> genScriptData)
      ]
  oneof
    [ pure $ TxOutDatumHash $ hashScriptDataBytes $ unsafeHashableScriptData scriptData
    , pure $ TxOutDatumInline $ unsafeHashableScriptData scriptData
    ]

genOutputFor ::
  forall ctx.
  VerificationKey PaymentKey ->
  Gen (TxOut ctx)
genOutputFor vk = do
  value <- genValue
  datum <- genDatum
  let out :: TxOut CtxUTxO
      out = TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value datum ReferenceScriptNone
  pure $ ensureMinAda out

genSigningKey :: Gen (SigningKey PaymentKey)
genSigningKey = do
  -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
  -- play well with pure generation from seed.
  sk <- fromJust . CC.rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 32 arbitrary
  pure (PaymentSigningKey sk)

-- | Generate some 'a' given the Party as a seed. NOTE: While this is useful to
-- generate party-specific values, it DOES depend on the generator used. For
-- example, `genForParty genVerificationKey` and `genForParty (fst <$>
-- genKeyPair)` do not yield the same verification keys!
genForParty :: Gen a -> Party -> a
genForParty gen Party{vkey} =
  generateWith gen seed
 where
  seed =
    fromIntegral
      . uintegerFromBytes
      . hydraKeyHashToBytes
      $ verificationKeyHash vkey

  hydraKeyHashToBytes (HydraKeyHash h) = hashToBytes h

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

instance Arbitrary TxId where
  arbitrary = onlyTxId <$> arbitrary
   where
    onlyTxId (TxIn txi _) = txi

genScriptRegistry :: Gen ScriptRegistry
genScriptRegistry = do
  txId' <- arbitrary
  vk <- arbitrary
  txOut <- genTxOutAdaOnly vk
  pure $
    ScriptRegistry
      { initialReference =
          ( TxIn txId' (TxIx 0)
          , txOut{txOutReferenceScript = mkScriptRef initialValidatorScript}
          )
      , commitReference =
          ( TxIn txId' (TxIx 1)
          , txOut{txOutReferenceScript = mkScriptRef commitValidatorScript}
          )
      , headReference =
          ( TxIn txId' (TxIx 2)
          , txOut{txOutReferenceScript = mkScriptRef Head.validatorScript}
          )
      , crsReference =
          ( TxIn txId' (TxIx 3)
          , txOut{txOutReferenceScript = mkScriptRef CRS.validatorScript, txOutDatum = createCRSG1Datum defaultItems}
          )
      }

instance Arbitrary Tx where
  -- TODO: shrinker!
  arbitrary = fromLedgerTx <$> arbitrary

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy fromList toList shrinkListAggressively

genHash :: Gen ByteString
genHash = BS.pack <$> vector 32

-- * Observations

instance Arbitrary HeadObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance ToADTArbitrary HeadObservation

instance Arbitrary InitObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AbortObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CommitObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CollectComObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DepositObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RecoverObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IncrementObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DecrementObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CloseObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ContestObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary FanoutObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary HeadSeed where
  arbitrary = UnsafeHeadSeed . BS.pack <$> vectorOf 16 arbitrary

instance Arbitrary HeadId where
  arbitrary = UnsafeHeadId . BS.pack <$> vectorOf 16 arbitrary

instance Arbitrary ContestationPeriod where
  arbitrary = do
    UnsafeContestationPeriod
      . fromInteger
      <$> oneof
        [ choose (1, confirmedHorizon)
        , pure confirmedHorizon
        , choose (confirmedHorizon, oneDay)
        , pure oneDay
        , pure oneWeek
        , pure oneMonth
        , pure oneYear
        ]
   where
    confirmedHorizon = 2160 * 20 -- k blocks on mainnet
    oneDay = 3600 * 24
    oneWeek = oneDay * 7
    oneMonth = oneDay * 30
    oneYear = oneDay * 365

-- | Parameter here is the contestation period (cp) so we need to generate
-- start (tMin) and end (tMax) tx validity bound such that their difference
-- is not higher than the cp.
-- Returned slots are tx validity bounds
genValidityBoundsFromContestationPeriod :: ContestationPeriod -> Gen (SlotNo, (SlotNo, UTCTime))
genValidityBoundsFromContestationPeriod cpSeconds = do
  startSlot@(SlotNo start) <- SlotNo <$> arbitrary
  let end = start + fromIntegral cpSeconds
  endSlot <- SlotNo <$> chooseWord64 (start, end)
  let time = slotNoToUTCTime systemStart slotLength endSlot
  pure (startSlot, (endSlot, time))

genPointInTimeBefore :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeBefore deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime systemStart slotLength deadline
  slot <- SlotNo <$> choose (0, slotDeadline)
  pure (slot, slotNoToUTCTime systemStart slotLength slot)

-- | Expect a given 'Tx' and 'UTxO' to pass evaluation.
propTransactionEvaluates :: (Tx, UTxO) -> Property
propTransactionEvaluates (tx, lookupUTxO) =
  case evaluateTx tx lookupUTxO of
    Left err ->
      property False
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Phase-1 validation failed: " <> show err)
    Right redeemerReport ->
      all isRight (Map.elems redeemerReport)
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Redeemer report:\n  " <> toString (renderEvaluationReport redeemerReport))
        & counterexample "Phase-2 validation failed"

-- | Expect a given 'Tx' and 'UTxO' to fail phase 1 or phase 2 evaluation.
propTransactionFailsEvaluation :: (Tx, UTxO) -> Property
propTransactionFailsEvaluation (tx, lookupUTxO) =
  case evaluateTx tx lookupUTxO of
    Left _ -> property True
    Right redeemerReport ->
      any isLeft redeemerReport
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Redeemer report: " <> show redeemerReport)
        & counterexample "Phase-2 validation should have failed"

instance Arbitrary (SigningKey HydraKey) where
  arbitrary = generateSigningKey . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary (VerificationKey HydraKey) where
  arbitrary = getVerificationKey <$> arbitrary

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (Signature a) where
  arbitrary = sign <$> arbitrary <*> arbitrary

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (MultiSignature a) where
  arbitrary = HydraMultiSignature <$> arbitrary

type ArbitraryIsTx tx =
  ( IsTx tx
  , Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (TxOutType tx)
  )

genOnChainId :: Gen OnChainId
genOnChainId = UnsafeOnChainId . BS.pack <$> vectorOf 28 arbitrary

instance Arbitrary OnChainId where
  arbitrary = genOnChainId

instance Arbitrary Party where
  arbitrary = genericArbitrary

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary
   where
    dedupParties HeadParameters{contestationPeriod, parties} =
      HeadParameters{contestationPeriod, parties = nub parties}

instance (Arbitrary tx, Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (Snapshot tx) where
  arbitrary = do
    headId <- arbitrary
    version <- arbitrary
    number <- arbitrary
    confirmed <- arbitrary
    utxo <- arbitrary
    utxoToCommit <- arbitrary
    utxoToDecommit <- arbitrary
    let accumulator = Accumulator.buildFromSnapshotUTxOs utxo utxoToCommit utxoToDecommit
    pure $ Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit, accumulator}

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink Snapshot{headId, version, number, utxo, confirmed, utxoToCommit, utxoToDecommit} =
    [ let accumulator = Accumulator.buildFromSnapshotUTxOs utxo' utxoToCommit' utxoToDecommit'
       in Snapshot headId version number confirmed' utxo' utxoToCommit' utxoToDecommit' accumulator
    | confirmed' <- shrink confirmed
    , utxo' <- shrink utxo
    , utxoToCommit' <- shrink utxoToCommit
    , utxoToDecommit' <- shrink utxoToDecommit
    ]

instance (Arbitrary tx, Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = do
    ks <- arbitrary
    utxo <- arbitrary
    utxoToCommit <- arbitrary
    utxoToDecommit <- arbitrary
    headId <- arbitrary
    genConfirmedSnapshot headId 0 0 utxo utxoToCommit utxoToDecommit ks

  shrink = \case
    InitialSnapshot hid sn -> [InitialSnapshot hid sn' | sn' <- shrink sn]
    ConfirmedSnapshot sn sigs -> ConfirmedSnapshot <$> shrink sn <*> shrink sigs

genConfirmedSnapshot ::
  IsTx tx =>
  HeadId ->
  -- | Exact snapshot version to generate.
  SnapshotVersion ->
  -- | The lower bound on snapshot number to generate.
  -- If this is 0, then we can generate an `InitialSnapshot` or a `ConfirmedSnapshot`.
  -- Otherwise we generate only `ConfirmedSnapshot` with a number strictly superior to
  -- this lower bound.
  SnapshotNumber ->
  UTxOType tx ->
  Maybe (UTxOType tx) ->
  Maybe (UTxOType tx) ->
  [SigningKey HydraKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot headId version minSn utxo utxoToCommit utxoToDecommit sks
  | minSn > 0 = confirmedSnapshot
  | otherwise =
      frequency
        [ (1, initialSnapshot)
        , (9, confirmedSnapshot)
        ]
 where
  initialSnapshot =
    InitialSnapshot <$> arbitrary <*> pure utxo

  confirmedSnapshot = do
    -- FIXME: This is another nail in the coffin to our current modeling of
    -- snapshots
    number <- arbitrary `suchThat` (> minSn)
    let u = utxo `withoutUTxO` fromMaybe mempty utxoToCommit
    let accumulator = Accumulator.buildFromSnapshotUTxOs u utxoToCommit utxoToDecommit
        snapshot = Snapshot{headId, version, number, confirmed = [], utxo = u, utxoToCommit, utxoToDecommit, accumulator}
    let signatures = aggregate $ fmap (`sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}

instance Arbitrary SnapshotNumber where
  arbitrary = genericArbitrary

instance Arbitrary SnapshotVersion where
  arbitrary = genericArbitrary

instance Arbitrary ChainSlot where
  arbitrary = genericArbitrary

instance Arbitrary UTxOHash where
  arbitrary = UTxOHash . BS.pack <$> vectorOf 32 arbitrary
