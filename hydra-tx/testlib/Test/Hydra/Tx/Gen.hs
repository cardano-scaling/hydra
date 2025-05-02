{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Tx.Gen where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.DSIGN qualified as CC
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Codec.CBOR.Magic (uintegerFromBytes)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import GHC.IsList (IsList (..))
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Tx (ScriptRegistry (..))
import Hydra.Tx.CollectCom (OpenThreadOutput)
import Hydra.Tx.Crypto (Hash (..))
import Hydra.Tx.Party (Party (..))
import PlutusTx.Builtins (fromBuiltin)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Hydra.Tx.Fixture qualified as Fixtures
import Test.QuickCheck (listOf, oneof, scale, shrinkList, shrinkMapBy, suchThat, vector, vectorOf)

-- * TxOut

instance Arbitrary (TxOut CtxUTxO) where
  arbitrary = genTxOut
  shrink txOut = fromLedgerTxOut <$> shrink (toLedgerTxOut txOut)

-- | Generate a 'Babbage' era 'TxOut', which may contain arbitrary assets
-- addressed to public keys and scripts, as well as datums.
--
-- NOTE: This generator does
--  * not produce byron addresses as most of the cardano ecosystem dropped support for that (including plutus),
--  * not produce reference scripts as they are not fully "visible" from plutus,
--  * replace stake pointers with null references as nobody uses that.
genTxOut :: Gen (TxOut ctx)
genTxOut =
  (noRefScripts . noStakeRefPtr <$> gen)
    `suchThat` notByronAddress
 where
  gen =
    modifyTxOutValue (<> (lovelaceToValue $ Coin 10_000_000))
      <$> oneof
        [ fromLedgerTxOut <$> arbitrary
        , notMultiAsset . fromLedgerTxOut <$> arbitrary
        ]

  notMultiAsset =
    modifyTxOutValue (lovelaceToValue . selectLovelace)

  notByronAddress (TxOut addr _ _ _) = case addr of
    ByronAddressInEra{} -> False
    _ -> True

  noStakeRefPtr out@(TxOut addr val dat refScript) = case addr of
    ShelleyAddressInEra (ShelleyAddress _ cre sr) ->
      case sr of
        Ledger.StakeRefPtr _ ->
          TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat refScript
        _ ->
          TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre sr)) val dat refScript
    _ -> out

  noRefScripts out =
    out{txOutReferenceScript = ReferenceScriptNone}

-- | Generate a 'TxOut' with a byron address. This is usually not supported by
-- Hydra or Plutus.
genTxOutByron :: Gen (TxOut ctx)
genTxOutByron = do
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ TxOut addr value TxOutDatumNone ReferenceScriptNone

-- | Generate an ada-only 'TxOut' payed to an arbitrary public key.
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

-- | Generate a complete arbitrary UTxO, which may contain arbitrary assets in
-- 'TxOut's addressed to public keys *and* scripts. NOTE: This is not reducing
-- size when generating assets in 'TxOut's, so will end up regularly with 300+
-- assets with generator size 30. NOTE: The Arbitrary TxIn instance from the
-- ledger is producing colliding values, so we replace them.
genUTxO :: Gen UTxO
genUTxO = do
  utxoMap <- Map.toList . Ledger.unUTxO <$> arbitrary
  fmap UTxO.fromList . forM utxoMap $ \(_, o) -> do
    i <- arbitrary
    pure (i, fromLedgerTxOut o)

-- | Generate a 'Babbage' era 'UTxO' with given number of outputs. See also
-- 'genTxOut'.
genUTxOSized :: Int -> Gen UTxO
genUTxOSized numUTxO =
  fold <$> vectorOf numUTxO (UTxO.singleton <$> gen)
 where
  gen = (,) <$> arbitrary <*> genTxOut

-- | Genereate a 'UTxO' with a single entry using given 'TxOut' generator.
genUTxO1 :: Gen (TxOut CtxUTxO) -> Gen UTxO
genUTxO1 gen = do
  txIn <- arbitrary
  txOut <- gen
  pure $ UTxO.singleton (txIn, txOut)

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
  fold <$> vectorOf numUTxO (UTxO.singleton <$> gen)
 where
  gen = (,) <$> arbitrary <*> (genTxOutAdaOnly =<< arbitrary)

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
  genEntry = (,) <$> genTxIn <*> genTxOut

-- * Others

instance Arbitrary AssetName where
  arbitrary = AssetName . BS.take 32 <$> arbitrary

genKeyPair :: Gen (VerificationKey PaymentKey, SigningKey PaymentKey)
genKeyPair = do
  sk <- genSigningKey
  pure (getVerificationKey sk, sk)

genValue :: Gen Value
genValue = fmap ((lovelaceToValue $ Coin 10_000_000) <>) (scale (`div` 10) $ fromLedgerValue <$> arbitrary)

genVerificationKey :: Gen (VerificationKey PaymentKey)
genVerificationKey = getVerificationKey <$> genSigningKey

-- | NOTE: See note on 'mkVkAddress' about 'NetworkId'.
genAddressInEra :: NetworkId -> Gen AddressInEra
genAddressInEra networkId =
  mkVkAddress networkId <$> genVerificationKey

-- TODO: Enable arbitrary datum in generators
-- TODO: This should better be called 'genOutputFor'
genOutput ::
  forall ctx.
  VerificationKey PaymentKey ->
  Gen (TxOut ctx)
genOutput vk = do
  value <- genValue
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone ReferenceScriptNone

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

instance Arbitrary OpenThreadOutput where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Tx where
  -- TODO: shrinker!
  arbitrary = fromLedgerTx <$> arbitrary

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy fromList toList shrinkListAggressively

genHash :: Gen ByteString
genHash = BS.pack <$> vector 32

-- | Generates value such that:
-- - alters between policy id we use in test fixtures with a random one.
-- - mixing arbitrary token names with 'hydraHeadV1'
-- - excluding 0 for quantity to mimic minting/burning
genMintedOrBurnedValue :: Gen Value
genMintedOrBurnedValue = do
  policyId <-
    oneof
      [ headPolicyId <$> arbitrary
      , pure Fixtures.testPolicyId
      ]
  tokenName <- oneof [arbitrary, pure (AssetName $ fromBuiltin hydraHeadV1)]
  quantity <- arbitrary `suchThat` (/= 0)
  pure $ fromList [(AssetId policyId tokenName, Quantity quantity)]
