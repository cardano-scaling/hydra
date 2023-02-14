{-# LANGUAGE TemplateHaskell #-}

-- | Data type used in 'Hydra.Contract.HeadTokens' as a separate module because
-- of TemplateHaskell stage restriction.
module Hydra.Contract.MintAction where

import qualified PlutusTx

-- TODO: remove this and use BuiltinBytestring or () to signal that we don't
-- need a redeemer
data MintAction = Mint | Burn

PlutusTx.unstableMakeIsData ''MintAction
