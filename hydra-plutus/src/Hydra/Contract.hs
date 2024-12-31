-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Codec.Serialise (serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Hydra.Cardano.Api (
  PlutusScriptVersion (PlutusScriptV3),
  ScriptHash,
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Hydra.Cardano.Api.Prelude qualified as Api
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Plutus (commitValidatorScript, depositValidatorScript, initialValidatorScript)
import Hydra.SerialisedScriptRegistry (SerialisedScriptRegistry (..))
import PlutusLedgerApi.V3 (TxId (..), TxOutRef (..), toBuiltin)

-- | Information about relevant Hydra scripts.
data ScriptInfo = ScriptInfo
  { mintingScriptHash :: ScriptHash
  -- ^ Hash of the μHead minting script given some default parameters.
  , mintingScriptSize :: Int64
  -- ^ Size of the μHead minting script given some default parameters.
  , initialScriptHash :: ScriptHash
  , initialScriptSize :: Int64
  , commitScriptHash :: ScriptHash
  , commitScriptSize :: Int64
  , headScriptHash :: ScriptHash
  , headScriptSize :: Int64
  , depositScriptHash :: ScriptHash
  , depositScriptSize :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Gather 'ScriptInfo' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
scriptInfo :: SerialisedScriptRegistry -> ScriptInfo
scriptInfo serialisedScriptRegistry =
  ScriptInfo
    { mintingScriptHash = plutusScriptHash $ HeadTokens.mintingPolicyScript defaultOutRef
    , mintingScriptSize = scriptSize $ HeadTokens.mintingPolicyScript defaultOutRef
    , initialScriptHash = hashScript $ Api.PlutusScript PlutusScriptV3 $ fromPlutusScript initialScriptValidator
    , initialScriptSize = scriptSize initialScriptValidator
    , commitScriptHash = hashScript $ Api.PlutusScript PlutusScriptV3 $ fromPlutusScript commitScriptValidator
    , commitScriptSize = scriptSize commitScriptValidator
    , headScriptHash = plutusScriptHash headScriptValidator
    , headScriptSize = scriptSize headScriptValidator
    , depositScriptHash = hashScript $ Api.PlutusScript PlutusScriptV3 $ fromPlutusScript depositScriptValidator
    , depositScriptSize = scriptSize depositScriptValidator
    }
 where
  SerialisedScriptRegistry
    { initialScriptValidator
    , commitScriptValidator
    , headScriptValidator
    , depositScriptValidator
    } = serialisedScriptRegistry

  plutusScriptHash =
    hashScript . PlutusScript . fromPlutusScript

  scriptSize = BSL.length . serialise

  defaultOutRef =
    TxOutRef
      { txOutRefId = TxId (toBuiltin . BS.pack $ replicate 32 0)
      , txOutRefIdx = 0
      }
