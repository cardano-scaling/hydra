#!/usr/bin/env bash
# A script for building everything in CI without having to sprinkle fragments of bash
# in a YAML file
# Assumes all needed tools are in scope which means it usually needs to be run
# from a nix-shell session

# fail the script if any command fails
set -e

cabal update

cabal build --enable-tests all

cabal test all

# Sanity check benchmark still runs fine
cabal bench local-cluster --benchmark-options '--scaling-factor 1'

# ignore various errors, including plutus scripts one
cabal haddock all -fhydra-development

[ ! -d docs/haddock ] && mkdir -p docs/haddock

doc_indices=$(find dist-newstyle/build  -name index.html)

for index in ${doc_indices}; do
  parent=$(dirname ${index})
  echo "Copying ${parent} to docs/haddock"
  cp -fr ${parent} docs/haddock
done
