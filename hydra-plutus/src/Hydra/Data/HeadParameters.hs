{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Data.HeadParameters where

import Ledger

import qualified PlutusTx

data HeadParameters = HeadParameters
  { participants :: [PubKeyHash]
  , policyId :: MintingPolicyHash
  }

PlutusTx.makeLift ''HeadParameters
