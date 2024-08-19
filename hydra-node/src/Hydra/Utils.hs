module Hydra.Utils where

import Crypto.Random (getRandomBytes)
import GHC.IO.Exception (userError)
import Hydra.Cardano.Api (File (..), FileError (FileIOError), Key (SigningKey), getVerificationKey, writeFileTextEnvelope)
import Hydra.Options (GenerateKeyPair (..))
import Hydra.Prelude
import Hydra.Tx.Crypto (HydraKey, generateSigningKey)
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
      sk :: SigningKey HydraKey <- generateSigningKey <$> getRandomBytes 16
      runExceptT $ do
        ExceptT $ writeFileTextEnvelope (File (outputFile <.> "sk")) Nothing sk
        ExceptT $ writeFileTextEnvelope (File (outputFile <.> "vk")) Nothing (getVerificationKey sk)
