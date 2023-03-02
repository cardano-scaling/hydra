{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "optics-th"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Andrzej Rybczak";
      homepage = "";
      url = "";
      synopsis = "Optics construction using TemplateHaskell";
      description = "This package is part of the @<https://hackage.haskell.org/package/optics optics>@\npackage family.  It provides machinery to construct optics using @TemplateHaskell@.\n\nSee the @template-haskell-optics@ package for optics to work with @template-haskell@ types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "optics-th-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
            (hsPkgs."optics-th" or (errorHandler.buildDepError "optics-th"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optics-th-0.4.1.tar.gz";
      sha256 = "d73857b79dcd8f7c7e70fa4727f134145b62902e8d3e448f8b25c38a9da4fd17";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\nname:          optics-th\nversion:       0.4.1\nx-revision: 2\nlicense:       BSD-3-Clause\nlicense-file:  LICENSE\nbuild-type:    Simple\nmaintainer:    optics@well-typed.com\nauthor:        Andrzej Rybczak\ntested-with:   GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7\n                || ==9.0.2 || ==9.2.2, GHCJS ==8.4\nsynopsis:      Optics construction using TemplateHaskell\ncategory:      Data, Optics, Lenses\ndescription:\n  This package is part of the @<https://hackage.haskell.org/package/optics optics>@\n  package family.  It provides machinery to construct optics using @TemplateHaskell@.\n  .\n  See the @template-haskell-optics@ package for optics to work with @template-haskell@ types.\n\nextra-doc-files:\n  CHANGELOG.md\n\nbug-reports:   https://github.com/well-typed/optics/issues\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/optics.git\n  subdir:   optics-th\n\ncommon language\n    ghc-options:        -Wall -Wcompat\n\n    default-language:   Haskell2010\n\n    default-extensions: BangPatterns\n                        ConstraintKinds\n                        DefaultSignatures\n                        DeriveFoldable\n                        DeriveFunctor\n                        DeriveGeneric\n                        DeriveTraversable\n                        EmptyCase\n                        FlexibleContexts\n                        FlexibleInstances\n                        FunctionalDependencies\n                        GADTs\n                        GeneralizedNewtypeDeriving\n                        InstanceSigs\n                        KindSignatures\n                        LambdaCase\n                        OverloadedLabels\n                        PatternSynonyms\n                        RankNTypes\n                        ScopedTypeVariables\n                        TupleSections\n                        TypeApplications\n                        TypeFamilies\n                        TypeOperators\n                        ViewPatterns\n\nlibrary\n  import:           language\n  hs-source-dirs:   src\n\n  build-depends: base                   >= 4.10      && <5\n               , containers             >= 0.5.10.2  && <0.7\n               , mtl                    >= 2.2.2     && <2.4\n               , optics-core            >= 0.4.1     && <0.5\n               , template-haskell       >= 2.12      && <2.20\n               , th-abstraction         >= 0.4       && <0.5\n               , transformers           >= 0.5       && <0.7\n\n  exposed-modules: Optics.TH\n\n                   -- internal modules\n                   Optics.TH.Internal.Utils\n                   Optics.TH.Internal.Product\n                   Optics.TH.Internal.Sum\n\n  other-modules:   Language.Haskell.TH.Optics.Internal\n\ntest-suite optics-th-tests\n  import:           language\n  hs-source-dirs:   tests\n\n  build-depends: base\n               , optics-core\n               , optics-th\n               , tagged\n\n  type:    exitcode-stdio-1.0\n  main-is: Optics/TH/Tests.hs\n\n  other-modules: Optics.TH.Tests.DuplicateRecordFields\n                 Optics.TH.Tests.T799\n";
    }