module Test.Hydra.Cluster.CardanoCliSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (generate)
import "aeson" Data.Aeson (encodeFile)
import "aeson" Data.Aeson.Types (parseEither)
import "base" System.Exit (ExitCode (..))
import "filepath" System.FilePath ((</>))
import "hydra-cardano-api" Hydra.Cardano.Api (LedgerEra, PParams, Tx)
import "hydra-node" Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import "hydra-node" Hydra.Chain.Direct (DirectBackend (..))
import "hydra-node" Hydra.JSONSchema (validateJSON, withJsonSpecifications)
import "hydra-node" Hydra.Logging (showLogsOnFailure)
import "hydra-node" Hydra.Options (

import CardanoNode (cliQueryProtocolParameters, withCardanoNodeDevnet)
  DirectOptions (..),
 )
import "lens" Control.Lens ((^?))
import "lens-aeson" Data.Aeson.Lens (key, _String)
import "process" System.Process (proc, readCreateProcessWithExitCode, readProcess)

spec :: Spec
spec =
  describe "cardano-cli" $ do
    it "cardano-cli can accept a draft commit tx in text-envelope format" $
      withTempDir "hydra-cluster" $ \tmpDir -> do
        let txFile = tmpDir </> "tx.raw"
        draftCommitResponse <- DraftCommitTxResponse <$> generate (arbitrary :: Gen Tx)
        encodeFile txFile draftCommitResponse

        (exitCode, output, _errors) <- readCreateProcessWithExitCode (cardanoCliSign txFile) ""
        exitCode `shouldBe` ExitSuccess
        output
          ^? key "type" . _String `shouldSatisfy` \case
            Nothing -> False
            Just something -> something == "Tx ConwayEra"

    it "has expected cardano-cli version available" $
      readProcess "cardano-cli" ["--version"] "" >>= (`shouldContain` "10.13.1.0")

    around (showLogsOnFailure "CardanoCliSpec") $ do
      it "query protocol-parameters is compatible with our FromJSON instance" $ \tracer ->
        withTempDir "hydra-cluster" $ \tmpDir -> do
          withCardanoNodeDevnet tracer tmpDir $ \_ backend -> do
            let DirectBackend DirectOptions{nodeSocket, networkId} = backend
            protocolParameters <- cliQueryProtocolParameters nodeSocket networkId
            case parseEither (parseJSON @(PParams LedgerEra)) protocolParameters of
              Left e -> failure $ "Failed to decode JSON: " <> e <> "\n" <> show protocolParameters
              Right _ -> pure ()

      it "query protocol-parameters matches our schema" $ \tracer ->
        withJsonSpecifications $ \tmpDir ->
          withCardanoNodeDevnet tracer tmpDir $ \_ backend -> do
            let DirectBackend DirectOptions{nodeSocket, networkId} = backend
            pparamsValue <- cliQueryProtocolParameters nodeSocket networkId
            validateJSON
              (tmpDir </> "api.json")
              (key "components" . key "schemas" . key "ProtocolParameters")
              pparamsValue
 where
  cardanoCliSign txFile =
    proc
      "cardano-cli"
      [ "conway"
      , "transaction"
      , "sign"
      , "--tx-file"
      , txFile
      , "--signing-key-file"
      , "config/credentials/alice.sk"
      , "--testnet-magic"
      , "42"
      , "--out-file"
      , "/dev/stdout"
      ]
