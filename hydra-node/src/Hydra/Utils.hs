module Hydra.Utils where

import Hydra.Prelude

import Hydra.Cardano.Api (File (..), FileError (FileIOError), Key (SigningKey), getVerificationKey, writeFileTextEnvelope)
import Hydra.Options (GenerateKeyPair (..))
import Hydra.Tx.Crypto (HydraKey, generateSigningKey)
import "aeson" Data.Aeson qualified as Aeson
import "aeson" Data.Aeson.Types qualified as Aeson
import "base" GHC.IO.Exception (userError)
import "crypto-api" Crypto.Random (getRandomBytes)
import "directory" System.Directory (doesFileExist)
import "filepath" System.FilePath ((<.>))

genHydraKeys :: GenerateKeyPair -> IO (Either (FileError ()) ())
genHydraKeys GenerateKeyPair{outputFile} = do
  fileExists <- doesFileExist outputFile
  if fileExists
    then
      pure $
        Left $
          FileIOError
            outputFile
            (userError "File already exists! Please remove it in order to generate new hydra keys.")
    else do
      sk :: SigningKey HydraKey <- generateSigningKey <$> getRandomBytes 16
      runExceptT $ do
        ExceptT $ writeFileTextEnvelope (File (outputFile <.> "sk")) Nothing sk
        ExceptT $ writeFileTextEnvelope (File (outputFile <.> "vk")) Nothing (getVerificationKey sk)

readJsonFileThrow :: (Aeson.Value -> Aeson.Parser a) -> FilePath -> IO a
readJsonFileThrow parser filepath = do
  value <- Aeson.eitherDecodeFileStrict filepath >>= either fail pure
  case Aeson.parseEither parser value of
    Left e -> fail e
    Right a -> pure a
