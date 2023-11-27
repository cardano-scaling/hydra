module Test.Hydra.Cluster.CardanoCliSpec where

import Hydra.Prelude hiding (toString)
import Test.Hydra.Prelude

import Control.Lens ((^?))
import Data.Aeson (encodeFile)
import Data.Aeson.Lens (key, _String)
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
        encodeFile txFile draftCommitResponse

        (exitCode, output, _errors) <- readCreateProcessWithExitCode (cardanoCliSign txFile) ""
        exitCode `shouldBe` ExitSuccess
        output
          ^? key "type" . _String `shouldSatisfy` \case
            Nothing -> False
            Just something -> something == "Witnessed Tx BabbageEra"
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
