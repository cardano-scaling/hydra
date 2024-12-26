module Hydra.SerialisedScriptRegistry where

import Hydra.Prelude

import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Base16 qualified as Base16

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import PlutusLedgerApi.Common (SerialisedScript)

data SerialisedScriptRegistry = SerialisedScriptRegistry
  { initialScriptValidator :: SerialisedScript
  , commitScriptValidator :: SerialisedScript
  , headScriptValidator :: SerialisedScript
  , depositScriptValidator :: SerialisedScript
  }
  deriving stock (Eq, Show)

serialisedScriptRegistry :: SerialisedScriptRegistry
serialisedScriptRegistry =
  SerialisedScriptRegistry
    { initialScriptValidator = initialValidatorScript
    , commitScriptValidator = commitValidatorScript
    , headScriptValidator = Head.validatorScript
    , depositScriptValidator = Deposit.validatorScript
    }

-- XXX: used to parse Aiken `compiledCode`.
serialisedScriptFromText :: Text -> SerialisedScript
serialisedScriptFromText base16Text =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode initial validator: " <> show e
    Right bytes -> toShort bytes
 where
  base16Bytes = encodeUtf8 base16Text

-- XXX: used to parse Plutus `cborHex`.
cborHexToSerialisedScript :: ByteString -> SerialisedScript
cborHexToSerialisedScript cborHex = either (error . show) SBS.toShort $ do
  bytes <- Base16.decode cborHex
  (_, a) <- first show $ CBOR.deserialiseFromBytes @ByteString fromCBOR (LBS.fromStrict bytes)
  pure a
