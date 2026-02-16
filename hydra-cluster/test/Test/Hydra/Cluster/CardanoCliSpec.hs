module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (cliQueryProtocolParameters, withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (encodeFile)
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types (parseEither)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import Hydra.Cardano.Api (LedgerEra, PParams, Tx)
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.JSONSchema (validateJSON, withJsonSpecifications)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Options (
  DirectOptions (..),
 )
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
            Just something -> something == "Tx ConwayEra"

    it "has expected cardano-cli version available" $
      readProcess "cardano-cli" ["--version"] "" >>= (`shouldContain` "10.15.0.0")

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
