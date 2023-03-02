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
    flags = { explicit-generic-labels = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "optics-core"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "optics@well-typed.com";
      author = "Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus";
      homepage = "";
      url = "";
      synopsis = "Optics as an abstract interface: core definitions";
      description = "This package makes it possible to define and use Lenses, Traversals, Prisms\nand other optics, using an abstract interface.\n\nThis variant provides core definitions with a minimal dependency footprint.\nSee the @<https://hackage.haskell.org/package/optics optics>@ package (and its\ndependencies) for documentation and the \"batteries-included\" variant.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."indexed-profunctors" or (errorHandler.buildDepError "indexed-profunctors"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optics-core-0.4.1.tar.gz";
      sha256 = "ca64e215f31719482a9449b5d56fbd589367e639d2b1869f7965a3970442949a";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\nname:          optics-core\nversion:       0.4.1\nx-revision: 1\nlicense:       BSD-3-Clause\nlicense-file:  LICENSE\nbuild-type:    Simple\nmaintainer:    optics@well-typed.com\nauthor:        Adam Gundry, Andres Löh, Andrzej Rybczak, Oleg Grenrus\ntested-with:   GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7\n                || ==9.0.2 || ==9.2.2, GHCJS ==8.4\nsynopsis:      Optics as an abstract interface: core definitions\ncategory:      Data, Optics, Lenses\ndescription:\n  This package makes it possible to define and use Lenses, Traversals, Prisms\n  and other optics, using an abstract interface.\n  .\n  This variant provides core definitions with a minimal dependency footprint.\n  See the @<https://hackage.haskell.org/package/optics optics>@ package (and its\n  dependencies) for documentation and the \"batteries-included\" variant.\n\nextra-doc-files:\n  diagrams/*.png\n  CHANGELOG.md\n\nbug-reports:   https://github.com/well-typed/optics/issues\nsource-repository head\n  type:     git\n  location: https://github.com/well-typed/optics.git\n  subdir:   optics-core\n\nflag explicit-generic-labels\n  default: False\n  manual:  True\n  description: Require explicit GenericLabelOptics instances in order to use generics-based labels\n\ncommon language\n    ghc-options:        -Wall -Wcompat\n\n    default-language:   Haskell2010\n\n    default-extensions: BangPatterns\n                        ConstraintKinds\n                        DefaultSignatures\n                        DeriveFoldable\n                        DeriveFunctor\n                        DeriveGeneric\n                        DeriveTraversable\n                        EmptyCase\n                        FlexibleContexts\n                        FlexibleInstances\n                        FunctionalDependencies\n                        GADTs\n                        GeneralizedNewtypeDeriving\n                        InstanceSigs\n                        KindSignatures\n                        LambdaCase\n                        OverloadedLabels\n                        PatternSynonyms\n                        RankNTypes\n                        ScopedTypeVariables\n                        TupleSections\n                        TypeApplications\n                        TypeFamilies\n                        TypeOperators\n                        ViewPatterns\n\nlibrary\n  import:           language\n  hs-source-dirs:   src\n\n  if flag(explicit-generic-labels)\n    cpp-options: -DEXPLICIT_GENERIC_LABELS\n\n  build-depends: base                   >= 4.10       && <5\n               , array                  >= 0.5.2.0    && <0.6\n               , containers             >= 0.5.10.2   && <0.7\n               , indexed-profunctors    >= 0.1        && <0.2\n               , transformers           >= 0.5        && <0.7\n               , indexed-traversable    >= 0.1        && <0.2\n\n  exposed-modules: Optics.Core\n\n                   -- main optic type\n                   Optics.Optic\n\n                   -- optic kinds\n                   Optics.AffineFold\n                   Optics.AffineTraversal\n                   Optics.Fold\n                   Optics.Getter\n                   Optics.Iso\n                   Optics.IxAffineFold\n                   Optics.IxAffineTraversal\n                   Optics.IxFold\n                   Optics.IxGetter\n                   Optics.IxLens\n                   Optics.IxSetter\n                   Optics.IxTraversal\n                   Optics.Lens\n                   Optics.Prism\n                   Optics.ReversedLens\n                   Optics.ReversedPrism\n                   Optics.Review\n                   Optics.Setter\n                   Optics.Traversal\n\n                   -- optic utilities\n                   Optics.Arrow\n                   Optics.At.Core\n                   Optics.Coerce\n                   Optics.Cons.Core\n                   Optics.Each.Core\n                   Optics.Empty.Core\n                   Optics.Generic\n                   Optics.Indexed.Core\n                   Optics.Mapping\n                   Optics.Label\n                   Optics.Operators\n                   Optics.Operators.Unsafe\n                   Optics.Re\n                   Optics.ReadOnly\n                   Optics.Core.Extras\n\n                   -- optics for data types\n                   Data.Either.Optics\n                   Data.IntMap.Optics\n                   Data.IntSet.Optics\n                   Data.List.Optics\n                   Data.Map.Optics\n                   Data.Maybe.Optics\n                   Data.Sequence.Optics\n                   Data.Set.Optics\n                   Data.Tree.Optics\n                   Data.Tuple.Optics\n                   Data.Typeable.Optics\n                   GHC.Generics.Optics\n                   Numeric.Optics\n\n                   -- internal modules\n                   Optics.Internal.Bi\n                   Optics.Internal.Fold\n                   Optics.Internal.Generic\n                   Optics.Internal.Generic.TypeLevel\n                   Optics.Internal.Indexed\n                   Optics.Internal.Indexed.Classes\n                   Optics.Internal.IxFold\n                   Optics.Internal.IxSetter\n                   Optics.Internal.IxTraversal\n                   Optics.Internal.Magic\n                   Optics.Internal.Optic\n                   Optics.Internal.Optic.Subtyping\n                   Optics.Internal.Optic.TypeLevel\n                   Optics.Internal.Optic.Types\n                   Optics.Internal.Setter\n                   Optics.Internal.Traversal\n                   Optics.Internal.Utils\n";
    }