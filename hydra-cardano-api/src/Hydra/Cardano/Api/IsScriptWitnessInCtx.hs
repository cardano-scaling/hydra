module Hydra.Cardano.Api.IsScriptWitnessInCtx where

import Hydra.Cardano.Api.Prelude

class IsScriptWitnessInCtx ctx where
  scriptWitnessCtx :: ScriptWitnessInCtx ctx

instance IsScriptWitnessInCtx WitCtxTxIn where
  scriptWitnessCtx = ScriptWitnessForSpending

instance IsScriptWitnessInCtx WitCtxMint where
  scriptWitnessCtx = ScriptWitnessForMinting

instance IsScriptWitnessInCtx WitCtxStake where
  scriptWitnessCtx = ScriptWitnessForStakeAddr
