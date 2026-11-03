-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.UserValidationRulesSpec where

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Data.Aeson.Types (object, parseEither, (.=))
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import GHC.IsList (toList)
import Hydra.Cardano.Api (
  AddressInEra,
  AssetId (..),
  CtxTx,
  Quantity (..),
  ScriptData (..),
  TxOut,
  fromLedgerAddr,
  getOriginalScriptDataBytes,
  getTxBody,
  getTxBodyContent,
  hashScriptDataBytes,
  txMintValue,
  txMintValueToValue,
  txOuts',
  unsafeHashableScriptData,
  pattern TxOut,
  pattern TxOutDatumHash,
  pattern TxOutDatumInline,
  pattern TxOutDatumNone,
  pattern TxOutSupplementalDatum,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, unsafeBuildTransaction)
import Hydra.Prelude hiding (toList)
import Hydra.UserValidationRules (
  MintingOnL2Rule (..),
  PolicyRule (..),
  PolicyViolation (..),
  UserValidation (..),
  UserValidationAction (..),
  UserValidationRules (..),
  cardanoUserValidation,
  checkAllowedScriptAddresses,
  checkDeposit,
  checkMaxDatumSize,
  checkMaxOutputValueAssets,
  checkPolicies,
  parsePolicyFile,
  validateUserActionWith,
  validateUserTransactionWith,
  validateUserUTxOWith,
 )
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (genTxOut, genTxWithMint, genUTxO)
import Test.QuickCheck (
  Property,
  checkCoverage,
  chooseInteger,
  conjoin,
  counterexample,
  coverTable,
  forAll,
  forAllBlind,
  generate,
  listOf1,
  oneof,
  resize,
  sublistOf,
  suchThat,
  tabulate,
  (===),
 )

instance Arbitrary PolicyRule where
  arbitrary = genericArbitrary

instance Arbitrary MintingOnL2Rule where
  arbitrary = genericArbitrary

instance Arbitrary UserValidationRules where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  describe "deposit policy configuration" $ do
    let parse :: ByteString -> Either String UserValidationRules
        parse bytes = first show (Yaml.decodeEither' bytes) >>= parseEither parsePolicyFile

    forM_ ["head-policy: {}", "head-policy: {deposits: null}"] $ \config ->
      it ("allows all deposits with " <> config) $ do
        rules <- either failure pure $ parse $ encodeUtf8 config
        depositId <- generate arbitrary
        allowedDeposits rules `shouldBe` Nothing
        validateDeposit (cardanoUserValidation rules) depositId `shouldBe` Right ()

    it "rejects all deposits with an empty list" $ do
      rules <- either failure pure $ parse "head-policy: {deposits: []}"
      depositId <- generate arbitrary
      validateDeposit (cardanoUserValidation rules) depositId `shouldBe` Left (DepositNotAllowed depositId)

    it "accepts only listed deposit IDs and ignores duplicates" $ do
      depositId <- generate arbitrary
      otherId <- generate $ arbitrary `suchThat` (/= depositId)
      rules <- either failure pure $ parse $ Yaml.encode $ object ["head-policy" .= object ["deposits" .= [depositId, depositId]]]
      allowedDeposits rules `shouldBe` Just (Set.singleton depositId)
      validateDeposit (cardanoUserValidation rules) depositId `shouldBe` Right ()
      validateDeposit (cardanoUserValidation rules) otherId `shouldBe` Left (DepositNotAllowed otherId)

    forM_ ["none-after-open", "true", "42", "{}", "[invalid-tx-id]"] $ \value ->
      it ("rejects an invalid deposits setting: " <> value) $
        parse (encodeUtf8 $ "head-policy: {deposits: " <> value <> "}") `shouldSatisfy` isLeft

  describe "complete UTxO policy" $ do
    let unrestricted = UserValidationRules Nothing AllowMinting Nothing Nothing Nothing Nothing Nothing
        check rules = validateResultingUTxO (cardanoUserValidation rules)
        outputs :: [Api.Value] -> IO Api.UTxO
        outputs values = do
          TxOut address _ _ _ <- generate (genTxOut :: Gen (TxOut Api.CtxUTxO))
          UTxO.fromList
            <$> forM
              values
              ( \value -> do
                  input <- generate arbitrary
                  pure (input, TxOut address value TxOutDatumNone Api.ReferenceScriptNone)
              )
        assetValue :: AssetId -> Api.Value
        assetValue asset = fromList [(AdaAssetId, Quantity 2000000), (asset, Quantity 1)]

    it "rejects a forbidden asset already held in the UTxO" $ do
      policy <- generate arbitrary
      utxo <- outputs [assetValue $ AssetId policy (Api.UnsafeAssetName "token")]
      check unrestricted{policyRule = Just $ AllowOnlyPolicies mempty} utxo `shouldSatisfy` isLeft
      check unrestricted{policyRule = Just $ AllowOnlyPolicies $ Set.singleton policy} utxo `shouldBe` Right ()

    it "counts distinct assets across all outputs" $ do
      policy <- generate arbitrary
      utxo <- outputs [assetValue $ AssetId policy (Api.UnsafeAssetName "one"), assetValue $ AssetId policy (Api.UnsafeAssetName "two")]
      check unrestricted{maxOutputValueAssets = Just 1} utxo
        `shouldBe` Left (MaxOutputValueAssetsExceeded 2 1)
      check unrestricted{maxOutputValueAssets = Just 2} utxo `shouldBe` Right ()

    it "counts a repeated asset only once and excludes ADA" $ do
      policy <- generate arbitrary
      utxo <- outputs $ replicate 2 (assetValue $ AssetId policy (Api.UnsafeAssetName "token"))
      check unrestricted{maxOutputValueAssets = Just 1} utxo `shouldBe` Right ()
      check unrestricted{maxUTxOCount = Just 1} utxo `shouldBe` Left (MaxUTxOCountExceeded 2 1)
      ada <- outputs [fromList [(AdaAssetId, Quantity 2000000)]]
      check unrestricted{maxOutputValueAssets = Just 0} ada `shouldBe` Right ()

    it "checks datums on outputs already held in the UTxO" $ do
      utxo <- outputs [fromList [(AdaAssetId, Quantity 2000000)]]
      let datum = unsafeHashableScriptData $ ScriptDataBytes "too large"
          withDatum = UTxO.map (\(TxOut address value _ script) -> TxOut address value (TxOutDatumInline datum) script) utxo
      check unrestricted{maxDatumSize = Just 0} withDatum `shouldSatisfy` isLeft

    it "keeps checking supplemental datum bytes carried by a transaction" $ do
      input <- generate arbitrary
      TxOut address _ _ _ <- generate (genTxOut :: Gen (TxOut CtxTx))
      let datum = unsafeHashableScriptData $ ScriptDataBytes "too large"
          output = TxOut address (Api.valueFromList [(AdaAssetId, 2000000)]) (TxOutSupplementalDatum datum) Api.ReferenceScriptNone
          tx = unsafeBuildTransaction $ Api.defaultTxBodyContent & addTxInsSpending [input] & Api.addTxOuts [output]
          validation = cardanoUserValidation unrestricted{maxDatumSize = Just 0}
      validateTransaction validation tx `shouldBe` checkMaxDatumSize 0 output

    it "checks script addresses on outputs already held in the UTxO" $ do
      scriptHash <- generate arbitrary
      utxo <- outputs [Api.valueFromList [(AdaAssetId, 2000000)]]
      let address = fromLedgerAddr $ Ledger.Addr Ledger.Testnet (Ledger.ScriptHashObj scriptHash) Ledger.StakeRefNull
          atScript = UTxO.map (\(TxOut _ value datum script) -> TxOut address value datum script) utxo
      check unrestricted{allowedScriptAddresses = Just mempty} atScript `shouldBe` Left (ScriptAddressNotAllowed address)
      check unrestricted{allowedScriptAddresses = Just $ Set.singleton address} atScript `shouldBe` Right ()

  describe "deposit policy handle" $ do
    prop "uses the configured deposit allowlist" $ \rules depositTxId ->
      validateDeposit (cardanoUserValidation rules) depositTxId
        === maybe (Right ()) (`checkDeposit` depositTxId) (allowedDeposits rules)
  describe "user validated transactions" $ do
    prop "checkPolicies" prop_checkMintedAssets
    prop "checkMaxOutputValueAssets" prop_checkMaxOutputValueAssets
    prop "checkMaxDatumSize" prop_checkMaxDatumSize
    prop "checkAllowedScriptAddresses" prop_checkAllowedScriptAddresses
    prop "checkDeposit" prop_checkDeposit
    prop "validateUserTransactionWith" prop_validateUserTransactionWith
    prop "validateUserActionWith" prop_validateUserActionWith

prop_validateUserActionWith :: Property
prop_validateUserActionWith =
  forAll genCase $ \(tx, utxo, rules, depositTxId) ->
    let checkAdmission rule expected =
          validateUserActionWith
            (rules{allowedDeposits = rule})
            utxo
            (AdmitNewDeposit depositTxId)
            === expected
        enabledRules =
          [ Set.empty
          , Set.singleton depositTxId
          ]
     in conjoin $
          [ counterexample "L2 transaction delegation" $
              validateUserActionWith rules utxo (ValidateL2Transaction tx)
                === validateUserTransactionWith (cardanoUserValidation rules) utxo tx
          , counterexample "deposit restriction disabled" $
              checkAdmission Nothing (validateUserUTxOWith rules utxo)
          ]
            <> [ counterexample ("deposit delegation: " <> show rule) $
                checkAdmission (Just rule) (checkDeposit rule depositTxId >> validateUserUTxOWith rules utxo)
               | rule <- enabledRules
               ]
 where
  genCase =
    (,,,) <$> genTxWithMint <*> genUTxO <*> arbitrary <*> arbitrary

prop_validateUserTransactionWith :: Property
prop_validateUserTransactionWith =
  forAll genCase $ \(tx, utxo, rules) ->
    tabulate "Policy rule" [policyCategory (policyRule rules)] $
      tabulate "L2 minting rule" [show (l2MintingRule rules)] $
        validateUserTransactionWith (cardanoUserValidation rules) utxo tx
          === expectedResult rules utxo tx
 where
  genCase =
    (,,) <$> genTxWithMint <*> genUTxO <*> arbitrary

  policyCategory = \case
    Nothing -> "policy restriction disabled"
    Just ForbidAnyPolicy -> "forbid all policies"
    Just (AllowOnlyPolicies _) -> "allow selected policies"

  expectedResult rules utxo tx = do
    case policyRule rules of
      Nothing -> Right ()
      Just ForbidAnyPolicy -> checkPolicies Set.empty tx
      Just (AllowOnlyPolicies allowed) -> checkPolicies allowed tx
    case l2MintingRule rules of
      ForbidMinting -> checkPolicies Set.empty tx
      AllowMinting -> Right ()
    for_ (maxDatumSize rules) $ \limit -> traverse_ (checkMaxDatumSize limit) (txOuts' tx)
    validateUserUTxOWith rules utxo

prop_checkMintedAssets :: Property
prop_checkMintedAssets =
  checkCoverage
    $ coverTable
      "Policy cases"
      [ ("no mint or burn", 20)
      , ("empty allowlist rejects", 20)
      , ("allowed policies accept", 20)
      ]
    $ forAll genTxWithMint
    $ \tx ->
      let mintedValue =
            txMintValueToValue . txMintValue . getTxBodyContent $
              getTxBody tx
          usedPolicies =
            Set.fromList
              [ policyId
              | (AssetId policyId _, quantity) <- toList mintedValue
              , quantity /= 0
              ]
       in forAll
            ( oneof
                [ pure Set.empty
                , pure usedPolicies
                , Set.fromList <$> sublistOf (Set.toList usedPolicies)
                ]
            )
            $ \allowed ->
              let expected = usedPolicies `Set.isSubsetOf` allowed
                  category
                    | Set.null usedPolicies = "no mint or burn"
                    | Set.null allowed = "empty allowlist rejects"
                    | expected = "allowed policies accept"
                    | otherwise = "partial allowlist rejects"
               in tabulate "Policy cases" [category] $
                    isRight (checkPolicies allowed tx) === expected

prop_checkMaxOutputValueAssets :: Property
prop_checkMaxOutputValueAssets =
  forAll genCase $ \(TxOut address _ datum refScript, policyId, names, quantity) ->
    let value =
          fromList $
            (AdaAssetId, Quantity 2000000)
              : [ (AssetId policyId name, Quantity quantity)
                | name <- Set.toList names
                ]
        output = TxOut address value datum refScript
        count = fromIntegral (Set.size names)
     in tabulate "Output assets" [if count == 0 then "ADA only" else "tokens"] $
          conjoin $
            [ checkMaxOutputValueAssets count [output] === Right ()
            , checkMaxOutputValueAssets (count + 1) [output] === Right ()
            ]
              <> [ checkMaxOutputValueAssets (count - 1) [output]
                  === Left
                    MaxOutputValueAssetsExceeded
                      { actualAssetCount = count
                      , maximumAssetCount = count - 1
                      }
                 | count > 0
                 ]
 where
  genCase = do
    output <- genTxOut :: Gen (TxOut CtxTx)
    policyId <- arbitrary
    names <-
      oneof
        [ pure Set.empty
        , Set.fromList <$> resize 12 (listOf1 arbitrary)
        ]
    quantity <- chooseInteger (1, 1000000)
    pure (output, policyId, names, quantity)

prop_checkMaxDatumSize :: Property
prop_checkMaxDatumSize =
  forAllBlind (genTxOut :: Gen (TxOut CtxTx)) $ \(TxOut address value _ refScript) ->
    forAll (arbitrary :: Gen [Word8]) $ \bytes ->
      let scriptData =
            unsafeHashableScriptData (ScriptDataBytes (BS.pack bytes))
          datumSize =
            fromIntegral (BS.length (getOriginalScriptDataBytes scriptData))
          output datum = TxOut address value datum refScript
          withOutput datum assertion =
            let actualOutput = output datum
             in counterexample
                  ("Output being checked: " <> show actualOutput)
                  $ assertion actualOutput
          checkPayload datum =
            withOutput datum $ \actualOutput ->
              counterexample ("Encoded datum bytes: " <> show datumSize) $
                conjoin $
                  [ checkMaxDatumSize datumSize actualOutput === Right ()
                  , checkMaxDatumSize (datumSize + 1) actualOutput === Right ()
                  ]
                    <> [ checkMaxDatumSize (datumSize - 1) actualOutput
                        === Left
                          MaxOutputDatumSizeExceeded
                            { actualDatumSize = datumSize
                            , maximumDatumSize = datumSize - 1
                            }
                       | datumSize > 0
                       ]
       in conjoin
            [ counterexample "TxOutDatumInline" $
                checkPayload (TxOutDatumInline scriptData)
            , counterexample "TxOutSupplementalDatum" $
                checkPayload (TxOutSupplementalDatum scriptData)
            , counterexample "TxOutDatumNone" $
                withOutput TxOutDatumNone $ \actualOutput ->
                  checkMaxDatumSize 0 actualOutput === Right ()
            , counterexample "TxOutDatumHash" $
                withOutput (TxOutDatumHash (hashScriptDataBytes scriptData)) $ \actualOutput ->
                  checkMaxDatumSize 0 actualOutput === Right ()
            ]

prop_checkAllowedScriptAddresses :: Property
prop_checkAllowedScriptAddresses =
  forAll (genTxOut :: Gen (TxOut CtxTx)) $ \(TxOut _ value datum refScript) ->
    forAll arbitrary $ \scriptHash ->
      forAll arbitrary $ \keyHash ->
        let scriptAddress =
              fromLedgerAddr $
                Ledger.Addr
                  Ledger.Testnet
                  (Ledger.ScriptHashObj scriptHash)
                  Ledger.StakeRefNull
            otherNetworkAddress =
              fromLedgerAddr $
                Ledger.Addr
                  Ledger.Mainnet
                  (Ledger.ScriptHashObj scriptHash)
                  Ledger.StakeRefNull
            stakedScriptAddress =
              fromLedgerAddr $
                Ledger.Addr
                  Ledger.Testnet
                  (Ledger.ScriptHashObj scriptHash)
                  (Ledger.StakeRefBase (Ledger.ScriptHashObj scriptHash))
            keyAddress =
              fromLedgerAddr $
                Ledger.Addr
                  Ledger.Testnet
                  (Ledger.KeyHashObj keyHash)
                  (Ledger.StakeRefBase (Ledger.ScriptHashObj scriptHash))
            output address = TxOut address value datum refScript
            rejected :: AddressInEra -> Either PolicyViolation ()
            rejected address =
              Left
                ScriptAddressNotAllowed
                  { rejectedScriptAddress = address
                  }
         in conjoin
              [ counterexample "listed script address" $
                  checkAllowedScriptAddresses
                    (Set.singleton scriptAddress)
                    (output scriptAddress)
                    === Right ()
              , counterexample "empty allowlist rejects script address" $
                  checkAllowedScriptAddresses Set.empty (output scriptAddress)
                    === rejected scriptAddress
              , counterexample "different network does not match" $
                  checkAllowedScriptAddresses
                    (Set.singleton otherNetworkAddress)
                    (output scriptAddress)
                    === rejected scriptAddress
              , counterexample "different staking component does not match" $
                  checkAllowedScriptAddresses
                    (Set.singleton stakedScriptAddress)
                    (output scriptAddress)
                    === rejected scriptAddress
              , counterexample "key payment with script staking is permitted" $
                  checkAllowedScriptAddresses Set.empty (output keyAddress)
                    === Right ()
              ]

prop_checkDeposit :: Property
prop_checkDeposit =
  forAll arbitrary $ \depositTxId ->
    forAll arbitrary $ \allowed ->
      let rejected =
            Left
              DepositNotAllowed
                { rejectedDepositTxId = depositTxId
                }
       in conjoin
            [ counterexample "empty allowlist" $
                checkDeposit Set.empty depositTxId
                  === rejected
            , counterexample "listed deposit" $
                checkDeposit
                  (Set.insert depositTxId allowed)
                  depositTxId
                  === Right ()
            , counterexample "unlisted deposit" $
                checkDeposit
                  (Set.delete depositTxId allowed)
                  depositTxId
                  === rejected
            ]
