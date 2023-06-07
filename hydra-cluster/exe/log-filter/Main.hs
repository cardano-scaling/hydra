{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS
import Hydra.LogFilter (tracePerformance)
import Hydra.Prelude
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  metavar,
  progDesc,
  str,
 )
import System.IO.Error (isEOFError)

newtype Options
  = FileInput (Maybe FilePath)

logFilterOptionsParser :: Parser Options
logFilterOptionsParser =
  FileInput <$> optional (argument str (metavar "FILE" <> help "The name of file to filter"))

logFilterOptions :: ParserInfo Options
logFilterOptions =
  info
    (logFilterOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Filter logs and compute events duration per transaction"
        <> header "log-filter - Hydra-node logs filter"
    )

main :: IO ()
main = do
  execParser logFilterOptions >>= \case
    FileInput (Just logFile) -> withFile logFile ReadMode $ \hdl -> go mempty hdl
    FileInput Nothing -> go mempty stdin
 where
  go pending hdl =
    try (LBS.hGetLine hdl) >>= \case
      Left err | isEOFError err -> pure ()
      Left err -> throwIO err
      Right line -> do
        case decode (LBS.fromStrict line) of
          Nothing -> go pending hdl
          Just e ->
            let (evs, pending') = runState (tracePerformance e) pending
             in mapM_ (LBS.hPutStrLn stdout . LBS.toStrict . encode) evs >> go pending' hdl
