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
      identifier = { name = "optics-extra"; version = "0.4.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Andrzej Rybczak";
      homepage = "";
      url = "";
      synopsis = "Extra utilities and instances for optics-core";
      description = "This package provides extra definitions and instances that extend the\n@<https://hackage.haskell.org/package/optics-core optics-core>@ package,\nwithout incurring too many dependencies.  See the\n@<https://hackage.haskell.org/package/optics optics>@ package for more\ndocumentation.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-profunctors" or (errorHandler.buildDepError "indexed-profunctors"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optics-extra-0.4.2.1.tar.gz";
      sha256 = "7e23a7a325e3448354614d3d958279c9ac2fdd0831ceee2808830e7a962fca41";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\nname:          optics-extra\nversion:       0.4.2.1\nx-revision: 1\nlicense:       BSD-3-Clause\nlicense-file:  LICENSE\nbuild-type:    Simple\nmaintainer:    optics@well-typed.com\nauthor:        Andrzej Rybczak\ntested-with:   GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7\n                || ==9.0.2 || ==9.2.3 || ==9.4.1, GHCJS ==8.4\nsynopsis:      Extra utilities and instances for optics-core\ncategory:      Data, Optics, Lenses\ndescription:\n  This package provides extra definitions and instances that extend the\n  @<https://hackage.haskell.org/package/optics-core optics-core>@ package,\n  without incurring too many dependencies.  See the\n  @<https://hackage.haskell.org/package/optics optics>@ package for more\n  documentation.\n\nextra-doc-files:\n  CHANGELOG.md\n\nbug-reports:   https://github.com/well-typed/optics/issues\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/optics.git\n  subdir:   optics-extra\n\ncommon language\n    ghc-options:        -Wall -Wcompat\n\n    default-language:   Haskell2010\n\n    default-extensions: BangPatterns\n                        ConstraintKinds\n                        DefaultSignatures\n                        DeriveFoldable\n                        DeriveFunctor\n                        DeriveGeneric\n                        DeriveTraversable\n                        EmptyCase\n                        FlexibleContexts\n                        FlexibleInstances\n                        FunctionalDependencies\n                        GADTs\n                        GeneralizedNewtypeDeriving\n                        InstanceSigs\n                        KindSignatures\n                        LambdaCase\n                        OverloadedLabels\n                        PatternSynonyms\n                        RankNTypes\n                        ScopedTypeVariables\n                        TupleSections\n                        TypeApplications\n                        TypeFamilies\n                        TypeOperators\n                        ViewPatterns\n\nlibrary\n  import:           language\n  hs-source-dirs:   src\n\n  build-depends: base                   >= 4.10      && <5\n               , array                  >= 0.5.2.0   && <0.6\n               , bytestring             >= 0.10.8    && <0.12\n               , containers             >= 0.5.10.2  && <0.7\n               , hashable               >= 1.1.1     && <1.5\n               , indexed-profunctors    >= 0.1       && <0.2\n               , mtl                    >= 2.2.2     && <2.4\n               , optics-core            >= 0.4.1     && <0.4.2\n               , text                   >= 1.2       && <1.3 || >=2.0 && <2.1\n               , transformers           >= 0.5       && <0.7\n               , unordered-containers   >= 0.2.6     && <0.3\n               , vector                 >= 0.11      && <0.14\n               , indexed-traversable-instances >=0.1 && <0.2\n\n  exposed-modules: Optics.Extra\n\n                   -- optic utilities\n                   Optics.At\n                   Optics.Cons\n                   Optics.Each\n                   Optics.Empty\n                   Optics.Indexed\n                   Optics.Passthrough\n                   Optics.State\n                   Optics.State.Operators\n                   Optics.View\n                   Optics.Zoom\n\n                   -- optics for data types\n                   Data.ByteString.Lazy.Optics\n                   Data.ByteString.Optics\n                   Data.ByteString.Strict.Optics\n                   Data.HashMap.Optics\n                   Data.HashSet.Optics\n                   Data.Text.Lazy.Optics\n                   Data.Text.Optics\n                   Data.Text.Strict.Optics\n                   Data.Vector.Generic.Optics\n                   Data.Vector.Optics\n\n                   -- internal modules\n                   Optics.Extra.Internal.ByteString\n                   Optics.Extra.Internal.Vector\n                   Optics.Extra.Internal.Zoom\n";
    }