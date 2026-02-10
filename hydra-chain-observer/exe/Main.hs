module Main where

import "hydra-prelude" Hydra.Prelude

import "hydra-chain-observer" Hydra.ChainObserver qualified

main :: IO ()
main = Hydra.ChainObserver.main
