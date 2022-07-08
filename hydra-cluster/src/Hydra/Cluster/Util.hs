-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
  Key (VerificationKey, getVerificationKey),
  PaymentKey,
  SigningKey,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
 )
import Hydra.Cluster.Fixture (Actor, actorName)
import Hydra.Options (ChainConfig (..), defaultChainConfig)
import qualified Paths_hydra_cluster as Pkg
import System.FilePath ((<.>), (</>))

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  filename <- Pkg.getDataFileName ("config" </> source)
  BS.readFile filename

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)
 where
  asSigningKey :: AsType (SigningKey PaymentKey)
  asSigningKey = AsSigningKey AsPaymentKey

chainConfigFor :: Actor -> FilePath -> FilePath -> [Actor] -> IO ChainConfig
chainConfigFor me targetDir nodeSocket them = do
  readConfigFile ("credentials" </> skName me) >>= writeFileBS (skTarget me)
  readConfigFile ("credentials" </> vkName me) >>= writeFileBS (vkTarget me)
  pure $
    defaultChainConfig
      { nodeSocket
      , cardanoSigningKey = skTarget me
      , cardanoVerificationKeys = [vkTarget himOrHer | himOrHer <- them]
      }
 where
  skTarget x = targetDir </> skName x
  vkTarget x = targetDir </> vkName x
  skName x = actorName x <.> ".sk"
  vkName x = actorName x <.> ".vk"
