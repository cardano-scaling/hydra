module Hydra.Utils where

import Crypto.Random (getRandomBytes)
import Hydra.Cardano.Api (File (..), FileError, Key (SigningKey), getVerificationKey, writeFileTextEnvelope)
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.Options (GenerateKeyPair (..))
import Hydra.Prelude
import System.FilePath ((<.>))

genHydraKeys :: GenerateKeyPair -> IO (Either (FileError ()) ())
genHydraKeys GenerateKeyPair{outputFile} = do
  sk :: SigningKey HydraKey <- generateSigningKey <$> getRandomBytes 16
  runExceptT $ do
    ExceptT $ writeFileTextEnvelope (File (outputFile <.> "sk")) Nothing sk
    ExceptT $ writeFileTextEnvelope (File (outputFile <.> "vk")) Nothing (getVerificationKey sk)