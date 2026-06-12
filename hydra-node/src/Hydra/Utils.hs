module Hydra.Utils where

import Hydra.Prelude

import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import GHC.IO.Exception (userError)
import Hydra.Cardano.Api (File (..), FileError (FileIOError), writeFileTextEnvelope)
import Hydra.Options (GenerateKeyPair (..))
import Hydra.Tx.Crypto (HydraKey, SigningKey, generateSigningKey, getVerificationKey)
import Hydra.Tx.Secret (Secret, withSecret)
import System.Directory (doesFileExist)
import System.FilePath ((<.>))

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
      sk :: Secret (SigningKey HydraKey) <- generateSigningKey <$> getRandomBytes 16
      runExceptT $ do
        ExceptT $ withSecret sk (writeFileTextEnvelope (File (outputFile <.> "sk")) Nothing)
        ExceptT $ writeFileTextEnvelope (File (outputFile <.> "vk")) Nothing (getVerificationKey sk)

readJsonFileThrow :: (Aeson.Value -> Aeson.Parser a) -> FilePath -> IO a
readJsonFileThrow parser filepath = do
  value <- Aeson.eitherDecodeFileStrict filepath >>= either fail pure
  case Aeson.parseEither parser value of
    Left e -> fail e
    Right a -> pure a
