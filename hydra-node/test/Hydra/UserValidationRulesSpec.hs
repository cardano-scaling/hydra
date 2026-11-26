{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
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
import Hydra.Tx (applyTxTo)
import Hydra.UserValidation.IncrementalValidation (cardanoUserValidation, prepareCardanoPolicyContext)
import Hydra.UserValidationRules (
  MintingOnL2Rule (..),
  PolicyContext (..),
  PolicyRule (..),
  PolicyViolation (..),
  UserValidation (..),
  UserValidationRules (..),
  checkAllowedScriptAddresses,
  checkDeposit,
  checkMaxDatumSize,
  checkMaxOutputValueAssets,
  checkPolicies,
  loadUserValidationRules,
  outputAssets,
  parsePolicyFile,
  preparePolicyContext,
  validateUserUTxOWith,
 )
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
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
  listOf,
  listOf1,
  oneof,
  resize,
  sublistOf,
  suchThat,
  tabulate,
  (===),
 )
import Prelude qualified

instance Arbitrary PolicyRule where
  arbitrary = genericArbitrary

instance Arbitrary MintingOnL2Rule where
  arbitrary = genericArbitrary

instance Arbitrary UserValidationRules where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  let parse :: ByteString -> Either String UserValidationRules
      parse bytes = first show (Yaml.decodeEither' bytes) >>= parseEither parsePolicyFile

  describe "policy configuration parsing" $ do
    it "defaults to unrestricted rules for an empty policy" $ do
      rules <- either failure pure $ parse "head-policy: {}"
      rules
        `shouldBe` UserValidationRules
          { policyRule = Nothing
          , l2MintingRule = AllowMinting
          , maxUTxOCount = Nothing
          , maxOutputValueAssets = Nothing
          , maxDatumSize = Nothing
          , allowedScriptAddresses = Nothing
          , allowedDeposits = Nothing
          }
    it "parses an empty asset allowlist as an empty set of policy IDs" $ do
      rules <- either failure pure $ parse "head-policy: {allowed-assets: []}"
      policyRule rules `shouldBe` Just (AllowOnlyPolicies Set.empty)
    it "parses an empty script address allowlist as an empty set" $ do
      rules <- either failure pure $ parse "head-policy: {allowed-script-addresses: []}"
      allowedScriptAddresses rules `shouldBe` Just Set.empty
    it "deduplicates policy IDs in the asset allowlist" $ do
      policyId <- generate (arbitrary :: Gen Api.PolicyId)
      let config =
            Yaml.encode $
              object
                [ "head-policy"
                    .= object
                      ["allowed-assets" .= [policyId, policyId]]
                ]
      rules <- either failure pure $ parse config
      policyRule rules `shouldBe` Just (AllowOnlyPolicies (Set.singleton policyId))

    it "preserves distinct policy IDs in the asset allowlist" $ do
      firstPolicy <- generate (arbitrary :: Gen Api.PolicyId)
      secondPolicy <- generate $ arbitrary `suchThat` (/= firstPolicy)
      let config = Yaml.encode $ object ["head-policy" .= object ["allowed-assets" .= [firstPolicy, secondPolicy]]]
      rules <- either failure pure $ parse config
      policyRule rules `shouldBe` Just (AllowOnlyPolicies (Set.fromList [firstPolicy, secondPolicy]))

    it "deduplicates script addresses in the script address allowlist" $ do
      scriptHash <- generate arbitrary
      let address = fromLedgerAddr $ Ledger.Addr Ledger.Testnet (Ledger.ScriptHashObj scriptHash) Ledger.StakeRefNull
          config = Yaml.encode $ object ["head-policy" .= object ["allowed-script-addresses" .= [address, address]]]
      rules <- either failure pure $ parse config
      allowedScriptAddresses rules `shouldBe` Just (Set.singleton address)

    it "accepts script payment addresses with absent, key, or script staking credentials" $ do
      paymentHash <- generate arbitrary
      stakingKeyHash <- generate arbitrary
      stakingScriptHash <- generate arbitrary
      let addresses =
            [ fromLedgerAddr $ Ledger.Addr Ledger.Testnet (Ledger.ScriptHashObj paymentHash) stake
            | stake <-
                [ Ledger.StakeRefNull
                , Ledger.StakeRefBase (Ledger.KeyHashObj stakingKeyHash)
                , Ledger.StakeRefBase (Ledger.ScriptHashObj stakingScriptHash)
                ]
            ]
          config = Yaml.encode $ object ["head-policy" .= object ["allowed-script-addresses" .= addresses]]
      rules <- either failure pure $ parse config
      allowedScriptAddresses rules `shouldBe` Just (Set.fromList addresses)

    it "rejects key payment addresses even when their staking credential is a script" $ do
      paymentHash <- generate arbitrary
      stakingScriptHash <- generate arbitrary
      forM_ [Ledger.StakeRefNull, Ledger.StakeRefBase (Ledger.ScriptHashObj stakingScriptHash)] $ \stake -> do
        let address :: AddressInEra
            address = fromLedgerAddr $ Ledger.Addr Ledger.Testnet (Ledger.KeyHashObj paymentHash) stake
            config = Yaml.encode $ object ["head-policy" .= object ["allowed-script-addresses" .= [address]]]
        parse config `shouldSatisfy` isLeft

    forM_ ["allowed-assets", "allowed-script-addresses"] $ \field ->
      forM_ ["null", "true", "42", "{}", "invalid", "[invalid]"] $ \value ->
        it ("rejects " <> field <> " set to " <> value) $
          parse (encodeUtf8 $ "head-policy: {" <> field <> ": " <> value <> "}") `shouldSatisfy` isLeft

    forM_
      [ ("unknown root fields", "head-policy: {}\nunknown: true")
      , ("unknown policy fields", "head-policy: {max-utxo-cout: 1}")
      , ("a missing head-policy", "{}")
      , ("a null document", "null")
      , ("a scalar document", "42")
      , ("a list document", "[]")
      ]
      $ \(description, config) ->
        it ("rejects " <> description) $
          parse config `shouldSatisfy` isLeft

    forM_ ["null", "42", "true", "policy", "[]"] $ \value ->
      it ("rejects a non-object head-policy: " <> value) $
        parse (encodeUtf8 $ "head-policy: " <> value) `shouldSatisfy` isLeft

    forM_ [("allowed", AllowMinting), ("forbidden", ForbidMinting)] $ \(value, expected) ->
      it ("parses l2-minting set to " <> value) $ do
        rules <- either failure pure $ parse $ encodeUtf8 $ "head-policy: {l2-minting: " <> value <> "}"
        l2MintingRule rules `shouldBe` expected

    forM_ ["null", "true", "42", "[]", "{}", "allow", "none-after-open"] $ \value ->
      it ("rejects an invalid l2-minting setting: " <> value) $
        parse (encodeUtf8 $ "head-policy: {l2-minting: " <> value <> "}") `shouldSatisfy` isLeft

    forM_
      [ ("max-utxo-count", maxUTxOCount)
      , ("max-output-value-assets", maxOutputValueAssets)
      , ("max-datum-bytes", maxDatumSize)
      ]
      $ \(field, getLimit) -> do
        forM_ [0, 1, 100] $ \limit ->
          it ("parses " <> field <> " set to " <> show limit) $ do
            rules <- either failure pure $ parse $ encodeUtf8 $ "head-policy: {" <> field <> ": " <> show limit <> "}"
            getLimit rules `shouldBe` Just limit
        forM_ ["-1", "1.5", "\"1\"", "null", "true", "[]", "{}"] $ \value ->
          it ("rejects " <> field <> " set to " <> value) $
            parse (encodeUtf8 $ "head-policy: {" <> field <> ": " <> value <> "}") `shouldSatisfy` isLeft

  describe "policy file loading" $ do
    it "loads configured rules from a YAML file" $
      withTempDir "user-policy" $ \dir -> do
        let path = dir </> "policy.yaml"
        BS.writeFile path "head-policy: {l2-minting: forbidden, max-utxo-count: 2, allowed-assets: [], allowed-script-addresses: [], deposits: []}"
        rules <- loadUserValidationRules path
        rules
          `shouldBe` UserValidationRules
            { policyRule = Just (AllowOnlyPolicies Set.empty)
            , l2MintingRule = ForbidMinting
            , maxUTxOCount = Just 2
            , maxOutputValueAssets = Nothing
            , maxDatumSize = Nothing
            , allowedScriptAddresses = Just Set.empty
            , allowedDeposits = Just Set.empty
            }

    it "exits with failure when the policy file does not exist" $
      withTempDir "user-policy" $ \dir ->
        loadUserValidationRules (dir </> "missing.yaml") `shouldThrow` (== ExitFailure 1)

    it "exits with failure when the policy path is a directory" $
      withTempDir "user-policy" $ \dir ->
        loadUserValidationRules dir `shouldThrow` (== ExitFailure 1)

    forM_
      [ ("malformed YAML", "head-policy: [")
      , ("invalid policy settings", "head-policy: {max-utxo-count: -1}")
      , ("an empty file", "")
      ]
      $ \(description, config) ->
        it ("exits with failure for " <> description) $
          withTempDir "user-policy" $ \dir -> do
            let path = dir </> "policy.yaml"
            BS.writeFile path config
            loadUserValidationRules path `shouldThrow` (== ExitFailure 1)

  describe "deposit policy configuration" $ do
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

    forM_ ["true", "42", "{}", "none-after-open", "[invalid-tx-id]"] $ \value ->
      it ("rejects an invalid deposits setting: " <> value) $
        parse (encodeUtf8 $ "head-policy: {deposits: " <> value <> "}") `shouldSatisfy` isLeft

  describe "complete UTxO policy" $ do
    let unrestricted = UserValidationRules Nothing AllowMinting Nothing Nothing Nothing Nothing Nothing
        check rules utxo = (preparePolicyContext (cardanoUserValidation rules) utxo mempty).policyResult
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

    it "keeps an asset counted until its last output is removed" $ do
      policy <- generate arbitrary
      let asset = AssetId policy (Api.UnsafeAssetName "shared")
      utxo <- outputs $ replicate 2 (assetValue asset)
      let rules = unrestricted{maxOutputValueAssets = Just 0}
          initial = prepareCardanoPolicyContext rules utxo mempty
          consume input = unsafeBuildTransaction $ Api.defaultTxBodyContent & addTxInsSpending [input]
          states = scanl (\current input -> current.applyPolicyTx $ consume input) initial (Map.keys $ UTxO.unUTxO utxo)
      map policyResult states `shouldBe` [Left (MaxOutputValueAssetsExceeded 1 0), Left (MaxOutputValueAssetsExceeded 1 0), Right ()]
      -- An invalid candidate does not alter the context it was derived from.
      initial.policyResult `shouldBe` Left (MaxOutputValueAssetsExceeded 1 0)

    it "rejects excess outputs before inspecting them for other policy rules" $ do
      input <- generate arbitrary
      let rules = unrestricted{maxUTxOCount = Just 0, maxDatumSize = Just 0}
          utxo = UTxO.UTxO $ LazyMap.singleton input (Prelude.error "unexpected output inspection")
      check rules utxo `shouldBe` Left (MaxUTxOCountExceeded 1 0)

    it "excludes outgoing outputs only from the UTxO count" $ do
      policy <- generate arbitrary
      outgoing <- outputs [assetValue $ AssetId policy (Api.UnsafeAssetName "one"), assetValue $ AssetId policy (Api.UnsafeAssetName "two")]
      let rules = unrestricted{maxUTxOCount = Just 0}
      (preparePolicyContext (cardanoUserValidation rules) mempty outgoing).policyResult `shouldBe` Right ()
      (preparePolicyContext (cardanoUserValidation rules{maxOutputValueAssets = Just 1}) mempty outgoing).policyResult
        `shouldBe` Left (MaxOutputValueAssetsExceeded 2 1)
      (preparePolicyContext (cardanoUserValidation rules{policyRule = Just $ AllowOnlyPolicies mempty}) mempty outgoing).policyResult
        `shouldBe` Left (PolicyAssetsViolation (Set.singleton policy) mempty)

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

  describe "incremental UTxO policy" $ do
    prop "matches full validation after every transaction and deposit, including overlapping outputs" $
      forAll genPolicyUpdates $ \(initial, outgoing, updates) ->
        let unrestricted = UserValidationRules Nothing AllowMinting Nothing Nothing Nothing Nothing Nothing
            allowed = Set.fromList [policy | AssetId policy _ <- Set.toList $ outputAssets $ UTxO.txOutputs outgoing]
            policies =
              [ unrestricted
              , unrestricted{maxUTxOCount = Just 3}
              , unrestricted{policyRule = Just ForbidAnyPolicy}
              , unrestricted{policyRule = Just $ AllowOnlyPolicies allowed}
              , unrestricted{maxOutputValueAssets = Just 2}
              , unrestricted{maxDatumSize = Just 0}
              , unrestricted{allowedScriptAddresses = Just mempty}
              , unrestricted{maxUTxOCount = Just 6, policyRule = Just ForbidAnyPolicy, maxOutputValueAssets = Just 2, maxDatumSize = Just 0, allowedScriptAddresses = Just mempty}
              ]
            check rules =
              let advance :: (Api.UTxO, PolicyContext Api.Tx) -> Either Api.UTxO Api.Tx -> (Api.UTxO, PolicyContext Api.Tx)
                  advance (utxo, policyContext) = \case
                    Left deposit -> (utxo <> deposit, policyContext.addPolicyUTxO deposit)
                    Right tx -> (applyTxTo tx utxo, policyContext.applyPolicyTx tx)
               in conjoin
                    [ counterexample ("rules: " <> show rules <> "; prefix: " <> show prefix) $
                      policyContext.policyResult === validateUserUTxOWith rules utxo outgoing
                    | (prefix, (utxo, policyContext)) <- zip [0 :: Int ..] $ scanl advance (initial, preparePolicyContext (cardanoUserValidation rules) initial outgoing) updates
                    ]
         in conjoin $ check <$> policies

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
        assets = outputAssets [output]
        count = fromIntegral (Set.size names)
     in tabulate "Output assets" [if count == 0 then "ADA only" else "tokens"] $
          conjoin $
            [ checkMaxOutputValueAssets count assets === Right ()
            , checkMaxOutputValueAssets (count + 1) assets === Right ()
            ]
              <> [ checkMaxOutputValueAssets (count - 1) assets
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

-- Spend existing references so removal, shared asset counts, and recovery from
-- invalid starting states are exercised as well as insertion. Deposits can
-- overlap both active and outgoing references.
genPolicyUpdates :: Gen (Api.UTxO, Api.UTxO, [Either Api.UTxO Api.Tx])
genPolicyUpdates = do
  initial <- resize 6 genUTxO
  outgoing <- resize 3 genUTxO
  shadowed <- UTxO.UTxO <$> traverse (const genTxOut) (UTxO.unUTxO initial)
  updates <- go 8 initial
  pure (initial, shadowed <> outgoing, updates)
 where
  go :: Int -> Api.UTxO -> Gen [Either Api.UTxO Api.Tx]
  go 0 _ = pure []
  go n current = do
    update <-
      oneof
        [ Left . (current <>) <$> resize 3 genUTxO
        , do
            inputs <- sublistOf $ Map.keys $ UTxO.unUTxO current
            outputs <- resize 3 $ listOf genTxOut
            pure . Right $ unsafeBuildTransaction $ Api.defaultTxBodyContent & addTxInsSpending inputs & Api.addTxOuts outputs
        ]
    let next = either (current <>) (`applyTxTo` current) update
    (update :) <$> go (n - 1) next
