#!/usr/bin/env bash
set -e


# public packages
cabal haddock \
  plutus-merkle-tree \
  plutus-cbor \
  hydra-prelude \
  hydra-cardano-api

# Internal packages
cabal haddock --haddock-tests \
  hydra-node
cabal haddock \
  hydra-tui \
  hydra-plutus \
  hydra-cluster

[ ! -d docs/static/haddock ] && mkdir -p docs/static/haddock

doc_indices=$(find dist-newstyle/build -name html -a -type d)

for index in ${doc_indices}; do
  echo "Copying ${index}/* to docs/static/haddock"
  cp -fr ${index}/* docs/static/haddock
done
