{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import Hydra.Cardano.Api (
  ScriptHash,
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial

-- | Information about relevant Hydra scripts.
data ScriptInfo = ScriptInfo
  { initialScriptHash :: ScriptHash
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
    { initialScriptHash = plutusScriptHash Initial.validatorScript
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
