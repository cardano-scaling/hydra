#!/usr/bin/env bash
# A 'simple' wrapper over ghcid and cabal that runs all hspecs
# from hydra-model and hydra-node

FLAGS=$(echo "
    -XBangPatterns
    -XBinaryLiterals
    -XConstraintKinds
    -XDataKinds
    -XDefaultSignatures
    -XDeriveDataTypeable
    -XDeriveFoldable
    -XDeriveFunctor
    -XDeriveGeneric
    -XDeriveTraversable
    -XDerivingStrategies
    -XEmptyDataDecls
    -XExistentialQuantification
    -XFlexibleContexts
    -XFlexibleInstances
    -XFunctionalDependencies
    -XGADTs
    -XInstanceSigs
    -XKindSignatures
    -XLambdaCase
    -XMultiParamTypeClasses
    -XMultiWayIf
    -XNamedFieldPuns
    -XNoImplicitPrelude
    -XNumericUnderscores
    -XOverloadedStrings
    -XPartialTypeSignatures
    -XPatternGuards
    -XRankNTypes
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XTupleSections
    -XTypeApplications
    -XTypeFamilies
    -XTypeOperators
    -XTypeSynonymInstances
    -XViewPatterns
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wpartial-fields
    -Wredundant-constraints
    -Wunused-packages" | tr -s '\012' ' ')

# List all hspec modules in the given packages and turn those into something
# that can be put into a Haskell list
SPECS=$(git ls-files 'hydra-model/**/*Spec.hs' 'hydra-node/**/*Spec.hs' | \
          sed -e 's/^[^A-Z]*\(.*\)\.hs$/\1/' | sed 'y=/=.=' | sed -e 's/$/.spec/' | tr -s '\012' ',' | sed -e 's/,$//' )

COMMAND="cabal exec ghci -- -ihydra-model/src -ihydra-model/test -ihydra-node/src -ihydra-node/test $FLAGS $(git ls-files 'hydra-model/**/*.hs' 'hydra-node/**/*.hs' | grep -v Main.hs| tr -s '\012' ' ')"

exec ghcid -c "$COMMAND" --restart=autotest.sh --restart=cabal.project \
     -T "mapM_ Test.Hspec.hspec [$SPECS]"
