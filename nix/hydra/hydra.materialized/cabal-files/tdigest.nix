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
      specVersion = "1.10";
      identifier = { name = "tdigest"; version = "0.2.1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/haskell-tdigest#readme";
      url = "";
      synopsis = "On-line accumulation of rank-based statistics";
      description = "A new data structure for accurate on-line accumulation of rank-based statistics such as quantiles and trimmed means.\n\nSee original paper: \"Computing extremely accurate quantiles using t-digest\" by Ted Dunning and Otmar Ertl\nfor more details <https://github.com/tdunning/t-digest/blob/07b8f2ca2be8d0a9f04df2feadad5ddc1bb73c88/docs/t-digest-paper/histo.pdf>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."reducers" or (errorHandler.buildDepError "reducers"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "tdigest-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tdigest" or (errorHandler.buildDepError "tdigest"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tdigest-0.2.1.1.tar.gz";
      sha256 = "a3998575ff5d180e6383d5bd5fc7c8e5fcfdb0c03e16f5f9089935a4d97173b7";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               tdigest\nversion:            0.2.1.1\nx-revision:         3\nsynopsis:           On-line accumulation of rank-based statistics\ndescription:\n  A new data structure for accurate on-line accumulation of rank-based statistics such as quantiles and trimmed means.\n  .\n  See original paper: \"Computing extremely accurate quantiles using t-digest\" by Ted Dunning and Otmar Ertl\n  for more details <https://github.com/tdunning/t-digest/blob/07b8f2ca2be8d0a9f04df2feadad5ddc1bb73c88/docs/t-digest-paper/histo.pdf>.\n\ncategory:           Numeric\nhomepage:           https://github.com/phadej/haskell-tdigest#readme\nbug-reports:        https://github.com/phadej/haskell-tdigest/issues\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.2\n\nbuild-type:         Simple\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/haskell-tdigest\n  subdir:   tdigest\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n\n  -- GHC boot libraries\n  build-depends:\n      base          >=4.7     && <4.19\n    , binary        >=0.7.1.0 && <0.10\n    , deepseq       >=1.3.0.2 && <1.5\n    , transformers  >=0.3     && <0.7\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.4 && <0.21\n\n  -- other dependencies\n  build-depends:\n      base-compat        >=0.10.1   && <0.13\n    , reducers           >=3.12.2   && <3.13\n    , semigroupoids      >=5.2.2    && <5.4\n    , vector             >=0.12.0.1 && <0.13\n    , vector-algorithms  >=0.7.0.1  && <0.9\n\n  exposed-modules:\n    Data.TDigest\n    Data.TDigest.NonEmpty\n    Data.TDigest.Postprocess\n    Data.TDigest.Tree\n    Data.TDigest.Tree.NonEmpty\n    Data.TDigest.Tree.Postprocess\n    Data.TDigest.Vector\n    Data.TDigest.Vector.NonEmpty\n    Data.TDigest.Vector.Postprocess\n\n  -- Internal modules are exposed, but aren't under PVP contract.\n  exposed-modules:\n    Data.TDigest.Internal\n    Data.TDigest.Postprocess.Internal\n    Data.TDigest.Tree.Internal\n    Data.TDigest.Vector.Internal\n\n  other-extensions:\n    DataKinds\n    KindSignatures\n    MultiParamTypeClasses\n    ScopedTypeVariables\n\ntest-suite tdigest-tests\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  ghc-options:      -Wall -threaded\n  hs-source-dirs:   tests\n  build-depends:\n      base\n    , base-compat\n    , binary\n    , deepseq\n    , semigroups\n    , tasty              >=0.11.0.4 && <1.5\n    , tasty-quickcheck   >=0.8.4    && <0.11\n    , tdigest\n    , vector\n    , vector-algorithms\n";
    }