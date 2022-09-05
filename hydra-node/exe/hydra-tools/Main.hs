module Main where

import Hydra.Prelude

import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Hydra.Cardano.Api (
  SigningKey,
  getVerificationKey,
  serialiseToRawBytes,
 )
import Hydra.Chain.Direct.Util (markerDatumHash)
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
  hsubparser,
  info,
  long,
  metavar,
  progDesc,
  strOption,
  value,
 )
import System.FilePath ((<.>))

data Options
  = GenerateKeyPair {outputFile :: FilePath}
  | OutputMarkerHash

outputFileParser :: Parser FilePath
outputFileParser =
  strOption
    ( long "output-file"
        <> metavar "FILE"
        <> value "hydra-key"
        <> help "Basename of files to generate key-pair into. Signing key will be suffixed '.sk' and verification key '.vk'"
    )

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "gen-hydra-key"
        ( info
            (helper <*> (GenerateKeyPair <$> outputFileParser))
            (progDesc "Generate a pair of Hydra signing/verification keys (off-chain keys).")
        )
        <> command
          "marker-hash"
          ( info
              (pure OutputMarkerHash)
              (progDesc "Output the hex-encoded hash of the marker datum used for fuel.")
          )
    )

toolsOptions :: ParserInfo Options
toolsOptions =
  info
    ( commandsParser
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
    OutputMarkerHash -> print markerDatumHash
