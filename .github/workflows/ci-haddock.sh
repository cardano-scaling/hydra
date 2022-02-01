#!/usr/bin/env bash
set -e

cabal haddock --haddock-tests all

[ ! -d docs/haddock ] && mkdir -p docs/haddock

doc_indices=$(find dist-newstyle/build  -name index.html)

for index in ${doc_indices}; do
  parent=$(dirname ${index})
  echo "Copying ${parent} to docs/haddock"
  cp -fr ${parent} docs/haddock
done
