-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  ScriptHash,
  hashScript,
  serialiseToRawBytes,
  pattern PlutusScript,
 )
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Plutus (commitValidatorScript, depositValidatorScript, initialValidatorScript)
import PlutusLedgerApi.V3 (TxId (..), TxOutRef (..), toBuiltin)

-- | Information about relevant Hydra scripts.
data HydraScriptCatalogue = HydraScriptCatalogue
  { mintingScriptHash :: ScriptHash
  -- ^ Hash of the μHead minting script given some default parameters.
  , mintingScriptSize :: Int
  -- ^ Size of the μHead minting script given some default parameters.
  , initialScriptHash :: ScriptHash
  , initialScriptSize :: Int
  , commitScriptHash :: ScriptHash
  , commitScriptSize :: Int
  , headScriptHash :: ScriptHash
  , headScriptSize :: Int
  , depositScriptHash :: ScriptHash
  , depositScriptSize :: Int
  , crsScriptHash :: ScriptHash
  , crsScriptSize :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Gather 'HydraScriptCatalogue' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
hydraScriptCatalogue :: HydraScriptCatalogue
hydraScriptCatalogue =
  HydraScriptCatalogue
    { mintingScriptHash = scriptHash $ HeadTokens.mintingPolicyScript defaultOutRef
    , mintingScriptSize = scriptSize $ HeadTokens.mintingPolicyScript defaultOutRef
    , initialScriptHash = scriptHash initialValidatorScript
    , initialScriptSize = scriptSize initialValidatorScript
    , commitScriptHash = scriptHash commitValidatorScript
    , commitScriptSize = scriptSize commitValidatorScript
    , headScriptHash = scriptHash Head.validatorScript
    , headScriptSize = scriptSize Head.validatorScript
    , depositScriptHash = scriptHash depositValidatorScript
    , depositScriptSize = scriptSize depositValidatorScript
    , crsScriptHash = scriptHash CRS.validatorScript
    , crsScriptSize = scriptSize CRS.validatorScript
    }
 where
  scriptHash = hashScript . PlutusScript

  scriptSize = BS.length . serialiseToRawBytes

  defaultOutRef =
    TxOutRef
      { txOutRefId = TxId (toBuiltin . BS.pack $ replicate 32 0)
      , txOutRefIdx = 0
      }
