module Hydra.Cardano.Api.StakeAddress where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Shelley (StakeCredential (StakeCredentialByScript))

-- | Construct a stake address from a Plutus script.
mkScriptStakeAddress ::
  forall lang.
  IsPlutusScriptLanguage lang =>
  NetworkId ->
  PlutusScript lang ->
  StakeAddress
mkScriptStakeAddress networkId script =
  makeStakeAddress networkId $ StakeCredentialByScript $ hashScript $ PlutusScript version script
 where
  version = plutusScriptVersion @lang
