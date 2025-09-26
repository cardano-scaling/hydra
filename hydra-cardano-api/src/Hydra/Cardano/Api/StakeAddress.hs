module Hydra.Cardano.Api.StakeAddress where

import Cardano.Api (IsPlutusScriptLanguage (..), NetworkId, PlutusScript, StakeAddress, hashScript, makeStakeAddress)
import Cardano.Api qualified as Api
import Cardano.Api.Shelley (StakeCredential (StakeCredentialByScript))

-- | Construct a stake address from a Plutus script.
mkScriptStakeAddress ::
  forall lang.
  IsPlutusScriptLanguage lang =>
  NetworkId ->
  PlutusScript lang ->
  StakeAddress
mkScriptStakeAddress networkId script =
  makeStakeAddress networkId $ StakeCredentialByScript $ hashScript $ Api.PlutusScript version script
 where
  version = plutusScriptVersion @lang
