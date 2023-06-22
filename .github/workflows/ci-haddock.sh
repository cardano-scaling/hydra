#!/usr/bin/env bash
set -e

# without --disable-documentation haddock will recompile all the things
# See https://github.com/haskell/cabal/issues/8707
cabal haddock --disable-documentation --haddock-tests all

[ ! -d docs/static/haddock ] && mkdir -p docs/static/haddock

doc_indices=$(find dist-newstyle/build -name html -a -type d)

for index in ${doc_indices}; do
  echo "Copying ${index}/* to docs/static/haddock"
  cp -fr ${index}/* docs/static/haddock
done
