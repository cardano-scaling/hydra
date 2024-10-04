module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import CardanoNode (cliQueryProtocolParameters, withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (encodeFile)
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types (parseEither)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import Hydra.Cardano.Api (LedgerEra, PParams, Tx)
import Hydra.JSONSchema (validateJSON, withJsonSpecifications)
import Hydra.Logging (showLogsOnFailure)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode, readProcess)
import Test.QuickCheck (generate)

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
            Just something -> something == "Witnessed Tx ConwayEra"

    it "is user tx valid" $
      withTempDir "hydra-cluster" $ \tmpDir -> do
        (exitCode, output, errors) <- readCreateProcessWithExitCode cardanoCliBuildTx ""
        traceShow errors $ exitCode `shouldBe` ExitSuccess
        output
          ^? key "type" . _String `shouldSatisfy` \case
            Nothing -> False
            Just something -> something == "Unwitnessed Tx BabbageEra"

    it "has expected cardano-cli version available" $
      readProcess "cardano-cli" ["--version"] "" >>= (`shouldContain` "9.4.1.0")

    around (showLogsOnFailure "CardanoCliSpec") $ do
      it "query protocol-parameters is compatible with our FromJSON instance" $ \tracer ->
        withTempDir "hydra-cluster" $ \tmpDir -> do
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId} -> do
            protocolParameters <- cliQueryProtocolParameters nodeSocket networkId
            case parseEither (parseJSON @(PParams LedgerEra)) protocolParameters of
              Left e -> failure $ "Failed to decode JSON: " <> e <> "\n" <> show protocolParameters
              Right _ -> pure ()

      it "query protocol-parameters matches our schema" $ \tracer ->
        withJsonSpecifications $ \tmpDir ->
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId} -> do
            pparamsValue <- cliQueryProtocolParameters nodeSocket networkId
            validateJSON
              (tmpDir </> "api.json")
              (key "components" . key "schemas" . key "ProtocolParameters")
              pparamsValue
 where
  cardanoCliSign txFile =
    proc
      "cardano-cli"
      [ "transaction"
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
  cardanoCliBuildTx =
    proc
      "cardano-cli"
      [ "transaction"
      , "build-raw"
      , "--fee"
      , "0"
      , "--protocol-params-file"
      , "/home/v0d1ch/code/hydra/hydra-cluster/config/protocol-parameters.json"
      , "--out-file"
      , "/home/v0d1ch/code/hydra/blueprint-with-withdraw.json"
      , "--tx-in"
      , "0b95bec9255a426435e6aa7ff9749d76ee4c6d815acb47f10142db889b0edce4#3"
      , "--withdrawal"
      , "stake_test17quu2gxsvfa2lfeg7ljd6yq59dmuy4up8sm02l3vhz8h9fg4g0qp2+0"
      , "--withdrawal-script-file"
      , "/home/v0d1ch/code/hydra/always-true.script"
      , "--certificate-script-file"
      , "/home/v0d1ch/code/hydra/always-true.script"
      , "--withdrawal-redeemer-value"
      , "\"Spend\""
      , "--withdrawal-execution-units"
      , "(1,1)"
      , "--tx-in-collateral"
      , "0b95bec9255a426435e6aa7ff9749d76ee4c6d815acb47f10142db889b0edce4#3"
      ]
