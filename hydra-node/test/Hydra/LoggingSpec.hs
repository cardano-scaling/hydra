module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (Tx)
import Hydra.Logging (LogFormat (..), traceWith, withTracerOutputTo, withTracerOutputToFormat)
import Hydra.Logging.Messages (HydraLog (..))
import System.FilePath ((</>))
import System.IO.Silently (capture_)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracerOutputTo LineBuffering stdout "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"

  it "writes CBOR logs starting with the self-described CBOR tag" $
    withTempDir "hydra-logging" $ \dir -> do
      let logFile = dir </> "log.cbor"
      withFile logFile WriteMode $ \hdl ->
        withTracerOutputToFormat CborFormat LineBuffering hdl "test" $ \tracer ->
          traceWith tracer (EnteringMainloop :: HydraLog Tx)
      bytes <- BS.readFile logFile
      -- 0xd9d9f7 is CBOR tag 55799 ("self-described CBOR"), doubling as file
      -- magic for `hydra-node convert-logs` format detection.
      BS.take 3 bytes `shouldBe` BS.pack [0xd9, 0xd9, 0xf7]
