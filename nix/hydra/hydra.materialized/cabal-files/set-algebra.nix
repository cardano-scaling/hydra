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
      identifier = { name = "set-algebra"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Set Algebra";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/set-algebra-0.1.0.0.tar.gz";
      sha256 = "0aa3a11548ce41e04ed20f3ed52d7609c2b717d5f6c316c55a7296465c61c1a9";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                set-algebra\nversion:             0.1.0.0\nsynopsis:            Set Algebra\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/set-algebra\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:     Control.Iterate.BaseTypes\n                     , Control.Iterate.Collect\n                     , Control.Iterate.Exp\n                     , Control.Iterate.SetAlgebra\n                     , Control.SetAlgebra\n\n  build-depends:       ansi-wl-pprint\n                     , base >=4.11 && <5\n                     , cardano-data\n                     , containers\n  hs-source-dirs:      src\n\ntest-suite tests\n  import:             project-config\n\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Control.Iterate.SetAlgebra\n                     , Test.Control.Iterate.RelationReference\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends:       base\n                     , containers\n                     , set-algebra\n                     , tasty\n                     , tasty-hunit\n                     , tasty-quickcheck\n                     , cardano-data\n  ghc-options:        -threaded\n";
    }