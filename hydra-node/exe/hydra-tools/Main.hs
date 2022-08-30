module Main where

import Hydra.Prelude

import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Hydra.Cardano.Api (
  SigningKey,
  TxId,
  getVerificationKey,
  serialiseToRawBytes,
 )
import Hydra.Crypto (HydraKey, generateSigningKey)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  progDesc,
  strOption,
  subparser,
  value,
 )
import System.FilePath ((<.>))

newtype Options = GenerateKeyPair {outputFile :: FilePath}

outputFileParser :: Parser Options
outputFileParser =
  GenerateKeyPair
    <$> strOption
      ( long "output-file"
          <> metavar "FILE"
          <> value "hydra-key"
          <> help "Basename of files to generate key-pair into. Signing key will be suffixed '.sk' and verification key '.vk'"
      )

optionsParser :: Parser Options
optionsParser =
  subparser $
    command
      "gen-hydra-key"
      ( info
          (helper <*> outputFileParser)
          (progDesc "Generate a pair of Hydra signing/verification keys (off-chain keys).")
      )

toolsOptions :: ParserInfo Options
toolsOptions =
  info
    ( optionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Utilities"
        <> header "hydra-tools - CLI to help working with Hydra"
    )

main :: IO ()
main = do
  opts <- execParser toolsOptions
  case opts of
    GenerateKeyPair{outputFile} -> do
      sk :: SigningKey HydraKey <- generateSigningKey <$> getRandomBytes 16
      BS.writeFile (outputFile <.> "sk") (serialiseToRawBytes sk)
      BS.writeFile (outputFile <.> "vk") (serialiseToRawBytes $ getVerificationKey sk)
