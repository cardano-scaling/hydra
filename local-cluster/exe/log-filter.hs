{-# LANGUAGE TypeApplications #-}

import Data.Aeson (Value, decode, encode)
import qualified Data.ByteString.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS
import Hydra.LogFilter (filterLog)
import Hydra.Prelude
import System.IO.Error (isEOFError)

main :: IO ()
main = do
  getArgs >>= \case
    [logFile] -> withFile logFile ReadMode $ \hdl -> go hdl
    _ -> go stdin
 where
  go hdl =
    try (LBS.hGetLine hdl) >>= \case
      Left err | isEOFError err -> pure ()
      Left err -> throwIO err
      Right line -> do
        case filterLog =<< decode @Value (LBS.fromStrict line) of
          Nothing -> pure ()
          Just v -> LBS.hPutStrLn stdout (LBS.toStrict $ encode v)
        go hdl
