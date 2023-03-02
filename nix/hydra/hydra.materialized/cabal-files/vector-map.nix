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
      identifier = { name = "vector-map"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "An efficient VMap that is backed by two vectors: one for keys and another for values.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-classes-base" or (errorHandler.buildDepError "quickcheck-classes-base"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/vector-map-0.1.0.0.tar.gz";
      sha256 = "9784f2a9082ce0abfdf9ef2c5e1c237f76be2dd820383e4eb52fff67e4169923";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                vector-map\nversion:             0.1.0.0\nsynopsis:            An efficient VMap that is backed by two vectors: one for keys and another for values.\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/vector-map\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             project-config\n\n  exposed-modules:     Data.VMap\n  other-modules:       Data.VMap.KVVector\n  build-depends:       base >=4.11 && <5\n                     , cardano-binary\n                     , containers\n                     , deepseq\n                     , nothunks\n                     , primitive\n                     , vector\n                     , vector-algorithms\n  hs-source-dirs:      src\n\ntest-suite tests\n  import:             project-config\n\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Common\n                     , Test.VMap\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends:       base\n                     , containers\n                     , tasty\n                     , tasty-quickcheck\n                     , vector-map\n                     , QuickCheck\n                     , quickcheck-classes-base\n  ghc-options:        -threaded\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Bench.hs\n  ghc-options:         -Wall -threaded -O2 -rtsopts\n  build-depends:       base\n                     , criterion\n                     , vector-map\n                     , containers\n                     , random\n  default-language:    Haskell2010\n\n";
    }