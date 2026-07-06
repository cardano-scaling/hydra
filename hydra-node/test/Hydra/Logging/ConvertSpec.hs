-- | Tests for @hydra-node convert-logs@, converting CBOR log streams back to
-- newline-delimited JSON (see "Hydra.Logging.Convert").
module Hydra.Logging.ConvertSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (Tx)
import Hydra.Logging (Envelope, encodeEnvelopeCbor, encodeEnvelopeJson)
import Hydra.Logging.Convert (ConvertResult (..), convertLogStream)
import Hydra.Logging.Messages (HydraLog)
import System.FilePath ((</>))
import Test.Hydra.Logging.Messages ()
import Test.QuickCheck (generate, resize)

spec :: Spec
spec = do
  it "converts a CBOR log stream to exactly the JSON lines the JSON tracer would have written" $ do
    envelopes <- generate $ resize 5 $ arbitrary @[Envelope (HydraLog Tx)]
    converted <- convert (foldMap encodeEnvelopeCbor envelopes)
    failedAt converted `shouldBe` Nothing
    converted' <- convertedOutput (foldMap encodeEnvelopeCbor envelopes)
    converted' `shouldBe` foldMap encodeEnvelopeJson envelopes

  it "converts the decodable prefix of a truncated stream and reports the offset" $ do
    envelope <- generate $ arbitrary @(Envelope (HydraLog Tx))
    let item = encodeEnvelopeCbor envelope
        truncated = item <> LBS.take (LBS.length item - 1) item
    result <- convert truncated
    converted result `shouldBe` 1
    failedAt result `shouldSatisfy` isJust

  it "passes JSON input through unchanged" $ do
    envelope <- generate $ arbitrary @(Envelope (HydraLog Tx))
    let jsonLines = encodeEnvelopeJson envelope
    output <- convertedOutput jsonLines
    output `shouldBe` jsonLines

  it "converts empty input to empty output" $ do
    result <- convert mempty
    result `shouldBe` ConvertResult{converted = 0, failedAt = Nothing}
 where
  convert input = fst <$> convertWithOutput input

  convertedOutput input = snd <$> convertWithOutput input

  convertWithOutput :: LBS.ByteString -> IO (ConvertResult, LBS.ByteString)
  convertWithOutput input =
    withTempDir "hydra-convert-logs" $ \dir -> do
      let inFile = dir </> "input"
          outFile = dir </> "output"
      LBS.writeFile inFile input
      result <-
        withFile inFile ReadMode $ \inH ->
          withFile outFile WriteMode $ \outH ->
            convertLogStream inH outH
      output <- LBS.readFile outFile
      _ <- evaluate (LBS.length output)
      pure (result, output)
