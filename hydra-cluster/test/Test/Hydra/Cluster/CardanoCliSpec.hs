module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude hiding (toString)
import Test.Hydra.Prelude

import Control.Lens ((^?))
import Data.Aeson (encode)
import Data.Aeson.Key (toString)
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy qualified as BS
import Data.Text (pack, unpack)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse))
import Hydra.Cardano.Api (Tx)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.QuickCheck (generate)

spec :: Spec
spec =
  describe "cardano-cli" $
    it "cardano-cli can accept a draft commit tx in text-envelope format" $
      withTempDir "cardano-cli" $ \tmpDir -> do
        let txFile = tmpDir </> "tx.raw"
        draftCommitResponse <- DraftCommitTxResponse <$> generate (arbitrary :: Gen Tx)
        let textEnvelope = encode $ toJSON draftCommitResponse
        _ <- BS.writeFile txFile textEnvelope

        (exitCode, output, _errors) <- readCreateProcessWithExitCode (cardanoCliSign txFile) ""
        exitCode `shouldBe` ExitSuccess
        findInOutput "type" "Witnessed Tx BabbageEra" output
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

  findInOutput lookFor expectedString output =
    case output ^? key lookFor . _String of
      Nothing ->
        failure $
          unpack $
            unlines $
              [ "Failed to find key " <> pack (toString lookFor) <> " in TextEnvelope."
              , "cardano-cli output:"
              , pack output
              ]
      Just foundString -> do
        foundString `shouldBe` expectedString
