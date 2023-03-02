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
      identifier = { name = "cardano-data"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Specialized data for Cardano project";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-data-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-data-0.1.0.0.tar.gz";
      sha256 = "84209ce188f0f8650cf6f4f724ad3234beba73183ba725eea5f6274a092a4f2b";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-data\nversion:             0.1.0.0\nsynopsis:            Specialized data for Cardano project\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/cardano-data\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:     Data.AbstractSize\n                     , Data.CanonicalMaps\n                     , Data.MemoBytes\n                     , Data.Coders\n                     , Data.Pulse\n                     , Data.Sharing\n                     , Data.BiMap\n                     , Data.MapExtras\n                     , Data.Roundtrip\n                     , Data.UMap\n\n  build-depends:       base >=4.11 && <5\n                     , bytestring\n                     , cborg\n                     , containers\n                     , vector-map\n                     , cryptonite\n                     , deepseq\n                     , formatting\n                     , mtl\n                     , microlens\n                     , nothunks\n                     , primitive\n                     , strict-containers\n                     , text\n                     , transformers >= 0.5\n                     -- IOHK deps\n                     , cardano-crypto-class\n                     , cardano-binary\n  hs-source-dirs:      src\n\n\ntest-suite cardano-data-tests\n  import:             project-config\n\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Data.UMap\n                     , Test.Data.Coders\n                     , Test.Data.MapExtras\n\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends:       base\n                     , bytestring\n                     , cardano-binary\n                     , cborg\n                     , containers\n                     , strict-containers\n                     , tasty\n                     , tasty-quickcheck\n                     , tasty-hunit\n                     , text\n                     , cardano-data\n                     , QuickCheck\n  ghc-options:        -threaded\n";
    }