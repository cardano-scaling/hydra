#!/usr/bin/env bash
set -e

# Run tests in sequence as integration tests do collide
cabal test plutus-cbor
cabal test plutus-merkle-tree
cabal test hydra-node
cabal test hydra-cluster
cabal test hydra-tui

# Sanity check benchmark still runs fine
# TODO: turn into a test
cabal bench hydra-cluster --benchmark-options '--scaling-factor 1'
