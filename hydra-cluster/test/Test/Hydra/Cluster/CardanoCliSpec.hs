module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude hiding (toString)
import Test.Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import CardanoClient (RunningNode (..))
import CardanoNode (withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (eitherDecode', encode, encodeFile)
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 (pack)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import Hydra.Cardano.Api (LedgerEra, Tx, unFile, unNetworkMagic)
import Hydra.Cardano.Api.Prelude (NetworkId (Testnet))
import Hydra.Logging (showLogsOnFailure)
import Hydra.Prelude (toString)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)
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

    around (showLogsOnFailure "CardanoCliSpec") $ do
      it "query PParams using cardano-node and check JSON roundtrip" $ \tracer ->
        withTempDir "queryProtocolParameters" $ \tmpDir ->
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{pparams} ->
            case eitherDecode' (encode pparams) :: Either String (PParams LedgerEra) of
              Left e -> expectationFailure e
              Right parsedPParams -> pparams `shouldBe` parsedPParams

      it "queried ProtocolParameters is compatible with our Json instance" $ \tracer ->
        withTempDir "cardano-cli-pparams" $ \tmpDir -> do
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId, pparams} -> do
            let magic =
                  case networkId of
                    Testnet networkId' -> unNetworkMagic networkId'
                    _ -> error "Should only run on Testnet"
            (exitCode, output, _errors) <- readCreateProcessWithExitCode (cardanoCliQueryPParams (toString $ unFile nodeSocket) magic) ""
            exitCode `shouldBe` ExitSuccess
            case eitherDecode' (pack output) :: Either String (PParams LedgerEra) of
              Left e -> expectationFailure e
              Right parsedPParams -> parsedPParams `shouldBe` pparams
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
      , show magic
      , "--out-file"
      , "/dev/stdout"
      ]
