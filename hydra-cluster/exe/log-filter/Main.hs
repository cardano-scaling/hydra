{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (Value, decode, encode)
import qualified Data.ByteString.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS
import Hydra.LogFilter (filterLog)
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
          "Filter and trim down logs from a Hydra node. This filter \
          \ keeps only the 'Node' messages, removes the networking layer ones, \
          \ and replaces full transactions in the log entries by transaction ids."
        <> header "log-filter - Raw Hydra-node logs filter"
    )

main :: IO ()
main = do
  execParser logFilterOptions >>= \case
    FileInput (Just logFile) -> withFile logFile ReadMode $ \hdl -> go hdl
    FileInput Nothing -> go stdin
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
