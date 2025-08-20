{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Tx.Gen where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.DSIGN qualified as CC
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Api (ensureMinCoinTxOut)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Codec.CBOR.Magic (uintegerFromBytes)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import GHC.IsList (IsList (..))
import Hydra.Contract.Head qualified as Head
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (ScriptRegistry (..))
import Hydra.Tx.Close (CloseObservation)
import Hydra.Tx.Crypto (Hash (..))
import Hydra.Tx.Observe (AbortObservation, CollectComObservation, CommitObservation, ContestObservation, DecrementObservation, DepositObservation, FanoutObservation, HeadObservation, IncrementObservation, InitObservation, RecoverObservation)
import Hydra.Tx.Party (Party (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Hydra.Tx.Fixture (pparams)
import Test.QuickCheck (listOf, listOf1, oneof, scale, shrinkList, shrinkMapBy, sized, suchThat, vector, vectorOf)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary (..))

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
      , notMultiAsset . fromLedgerTxOut <$> arbitrary
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

genTxOutWithAssets :: Gen (TxOut ctx)
genTxOutWithAssets =
  ((fromLedgerTxOut <$> arbitrary) `suchThat` notByronAddress)
    >>= realisticAda
    <&> ensureSomeAda . noRefScripts . noStakeRefPtr

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

genUTxOWithAssetsSized :: Int -> Gen UTxO
genUTxOWithAssetsSized numUTxO =
  fold <$> vectorOf numUTxO gen
 where
  gen :: Gen UTxO
  gen = UTxO.singleton <$> arbitrary <*> genTxOutWithAssets

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
  outs <- vectorOf n (genOutput vk)
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
  output <- scale (const 1) $ genOutput vk
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
  arbitrary = AssetName . BS.take 32 <$> arbitrary

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

-- TODO: This should better be called 'genOutputFor'
genOutput ::
  forall ctx.
  VerificationKey PaymentKey ->
  Gen (TxOut ctx)
genOutput vk = do
  value <- genValue
  datum <- genDatum
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value datum ReferenceScriptNone

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
