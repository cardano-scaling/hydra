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
    -fno-ignore-interface-pragmas
    -fno-omit-interface-pragmas
    -fplugin-opt PlutusTx.Plugin:defer-errors
    -fobject-code
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
SPECS=$(git ls-files  'hydra-node/**/*Spec.hs'  'hydra-plutus/**/*Spec.hs' | \
          sed -e 's/^[^A-Z]*\(.*\)\.hs$/\1/' | sed 'y=/=.=' | sed -e 's/$/.spec/' | tr -s '\012' ',' | sed -e 's/,$//' )

COMMAND="cabal exec ghci -- -ihydra-plutus/src -ihydra-plutus/test -ihydra-node/src -ihydra-node/test $FLAGS $(git ls-files 'hydra-node/**/*.hs'  'hydra-plutus/**/*.hs' | grep -v Main.hs| grep -v Repl | tr -s '\012' ' ')"

# need to explicitly list *.cabal files to restart because (I think) ghcid only
# checks .cabal in current directory
exec ghcid -c "$COMMAND" --restart=autotest.sh --restart=cabal.project \
     --restart=hydra-node/hydra-node.cabal \
     --restart=hydra-plutus/hydra-plutus.cabal \
    -T "Control.Monad.mapM_ Test.Hspec.hspec [$SPECS]"
