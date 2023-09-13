{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Hydra.Cardano.Api (
  ScriptHash,
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import qualified Hydra.Contract.Initial as Initial
import Hydra.Plutus (commitValidatorScript)
import PlutusLedgerApi.V2 (TxId (..), TxOutRef (..), toBuiltin)

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
  deriving (Eq, Show, Generic, ToJSON)

-- | Gather 'ScriptInfo' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
scriptInfo :: ScriptInfo
scriptInfo =
  ScriptInfo
    { mintingScriptHash = plutusScriptHash $ HeadTokens.mintingPolicyScript defaultOutRef
    , mintingScriptSize = scriptSize $ HeadTokens.mintingPolicyScript defaultOutRef
    , initialScriptHash = plutusScriptHash Initial.validatorScript
    , initialScriptSize = scriptSize Initial.validatorScript
    , commitScriptHash = plutusScriptHash commitValidatorScript
    , commitScriptSize = scriptSize commitValidatorScript
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
