module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import CardanoNode (withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (eitherDecode', encodeFile)
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 (pack)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import Hydra.Cardano.Api (ProtocolParameters, Tx, unFile, unNetworkMagic)
import Hydra.Cardano.Api.Prelude (NetworkId (Testnet))
import Hydra.Logging (showLogsOnFailure)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode, readProcess)
import Test.QuickCheck (generate)

spec :: Spec
spec =
  describe "cardano-cli" $ do
    it "cardano-cli can accept a draft commit tx in text-envelope format" $
      withTempDir "cardano-cli" $ \tmpDir -> do
        let txFile = tmpDir </> "tx.raw"
        draftCommitResponse <- DraftCommitTxResponse <$> generate (arbitrary :: Gen Tx)
        encodeFile txFile draftCommitResponse

        (exitCode, output, _errors) <- readCreateProcessWithExitCode (cardanoCliSign txFile) ""
        exitCode `shouldBe` ExitSuccess
        output
          ^? key "type" . _String `shouldSatisfy` \case
            Nothing -> False
            Just something -> something == "Witnessed Tx BabbageEra"

    it "has expected cardano-cli version available" $
      readProcess "cardano-cli" ["--version"] "" >>= (`shouldContain` "8.17.0.0")

    around (showLogsOnFailure "CardanoCliSpec") $ do
      it "query protocol-parameters is compatible with our FromJSON instance" $ \tracer ->
        withTempDir "cardano-cli-pparams" $ \tmpDir -> do
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId} -> do
            case networkId of
              Testnet networkId' -> do
                (exitCode, output, _errors) <-
                  readCreateProcessWithExitCode
                    (cardanoCliQueryPParams (unFile nodeSocket) (show $ unNetworkMagic networkId'))
                    ""
                exitCode `shouldBe` ExitSuccess
                case eitherDecode' (pack output) :: Either String ProtocolParameters of
                  Left e -> failure $ "Failed to decode JSON: " <> e <> "\n" <> output
                  Right _ -> pure ()
              _ -> failure "Should only run on Testnet"

      it "query protocol-parameters matches our schema" $ \_tracer ->
        pendingWith "TODO"
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

  cardanoCliQueryPParams nodeSocket magic =
    proc
      "cardano-cli"
      [ "query"
      , "protocol-parameters"
      , "--socket-path"
      , nodeSocket
      , "--testnet-magic"
      , magic
      , "--out-file"
      , "/dev/stdout"
      ]
