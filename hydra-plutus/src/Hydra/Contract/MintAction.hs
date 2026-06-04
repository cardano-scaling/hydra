{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Data type used in 'Hydra.Contract.HeadTokens' as a separate module because
-- of TemplateHaskell stage restriction.
module Hydra.Contract.MintAction where

import PlutusTx qualified

-- | Redeemer for the head minting policy.
--
--   * 'Mint'  — initial Head init: mints the ST plus one PT per party.
--   * 'Burn'  — closure path: burns the remaining head tokens.
--   * 'MintParticipant' — Phase 2 of dynamic-head-participants (issue #1813).
--     Mints a single PT for a joining party. The head validator's
--     'UpdateParameters'/'AddPartyOC' branch is what authorizes this; here we
--     only verify that the head input is being spent with that redeemer.
data MintAction = Mint | Burn | MintParticipant

PlutusTx.unstableMakeIsData ''MintAction
