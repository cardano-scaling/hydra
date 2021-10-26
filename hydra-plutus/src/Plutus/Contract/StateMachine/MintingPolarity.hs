{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Data type used for minting and burning the thread token value.
--
--  ''NOTE'': Vendored from https://github.com/input-output-hk/plutus-apps/tree/d0fd9d49e6e862dc5abed41f0f07f56aafb652cf/plutus-contract/src/Plutus/Contract/StateMachine/MintingPolarity.hs
module Plutus.Contract.StateMachine.MintingPolarity where

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

data MintingPolarity = Mint | Burn deriving (Haskell.Eq, Haskell.Show)

PlutusTx.makeIsDataIndexed ''MintingPolarity [('Mint, 0), ('Burn, 1)]

instance Eq MintingPolarity where
  Mint == Mint = True
  Burn == Burn = True
  _ == _ = False
