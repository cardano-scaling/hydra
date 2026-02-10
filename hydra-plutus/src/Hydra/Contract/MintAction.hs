{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Data type used in 'Hydra.Contract.HeadTokens' as a separate module because
-- of TemplateHaskell stage restriction.
module Hydra.Contract.MintAction where

import "plutus-tx" PlutusTx qualified

data MintAction = Mint | Burn

PlutusTx.unstableMakeIsData ''MintAction
