#!/usr/bin/env bash
# A script for building everything in CI without having to sprinkle fragments of bash
# in a YAML file
# Assumes all needed tools are in scope which means it usually needs to be run
# from a nix-shell session

# fail the script if any command fails
set -e

cabal update

cabal build --enable-tests all

# Run tests in sequence as integration tests do collide
cabal test merkle-patricia-tree
cabal test plutus-cbor
cabal test hydra-plutus
cabal test hydra-node
cabal test hydra-cluster
cabal test hydra-tui

# Sanity check benchmark still runs fine
# TODO: turn into a test
cabal bench hydra-cluster --benchmark-options '--scaling-factor 1'

# ignore various errors, including plutus scripts one
cabal haddock all -fhydra-development

[ ! -d docs/haddock ] && mkdir -p docs/haddock

doc_indices=$(find dist-newstyle/build  -name index.html)

for index in ${doc_indices}; do
  parent=$(dirname ${index})
  echo "Copying ${parent} to docs/haddock"
  cp -fr ${parent} docs/haddock
done
