-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Codec.Serialise (serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Hydra.Cardano.Api (
  ScriptHash,
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
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
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Gather 'ScriptInfo' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
scriptInfo :: ScriptInfo
scriptInfo =
  ScriptInfo
    { mintingScriptHash = plutusScriptHash $ HeadTokens.mintingPolicyScript defaultOutRef
    , mintingScriptSize = scriptSize $ HeadTokens.mintingPolicyScript defaultOutRef
    , initialScriptHash = plutusScriptHash Initial.validatorScript
    , initialScriptSize = scriptSize Initial.validatorScript
    , commitScriptHash = plutusScriptHash Commit.validatorScript
    , commitScriptSize = scriptSize Commit.validatorScript
    , headScriptHash = plutusScriptHash Head.validatorScript
    , headScriptSize = scriptSize Head.validatorScript
    }
 where
  plutusScriptHash =
    hashScript . PlutusScript . fromPlutusScript

  scriptSize = BSL.length . serialise

  defaultOutRef =
    TxOutRef
      { txOutRefId = TxId (toBuiltin . BS.pack $ replicate 32 0)
      , txOutRefIdx = 0
      }
