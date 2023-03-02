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
    flags = { development = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-slotting"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Key slotting types for cardano libraries";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-slotting-0.1.0.0.tar.gz";
      sha256 = "3748d4496ee6ad8ea9cf1ee4d29d32d0f26048e6349ef768482455f183cdb597";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                cardano-slotting\nversion:             0.1.0.0\nsynopsis:            Key slotting types for cardano libraries\n-- description:\n-- bug-reports:\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           IOHK\n-- category:\nbuild-type:          Simple\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\ncommon base                         { build-depends: base                             >= 4.14       && < 4.15     }\n\ncommon project-config\n  default-language:     Haskell2010\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wredundant-constraints\n                        -Wunused-packages\n\n  if (!flag(development))\n    ghc-options:        -Werror\n\nlibrary\n  import:               base, project-config\n  hs-source-dirs:       src\n\n  exposed-modules:\n                        Cardano.Slotting.Block\n                        Cardano.Slotting.EpochInfo\n                        Cardano.Slotting.EpochInfo.API\n                        Cardano.Slotting.EpochInfo.Extend\n                        Cardano.Slotting.EpochInfo.Impl\n                        Cardano.Slotting.Slot\n                        Cardano.Slotting.Time\n\n  build-depends:        aeson\n                      , base\n                      , cardano-binary\n                      , cborg\n                      , deepseq\n                      , mmorph\n                      , nothunks\n                      , quiet\n                      , serialise\n                      , time             >=1.9.1 && <1.11\n\ntest-suite tests\n  import:               base, project-config\n  type:                 exitcode-stdio-1.0\n  hs-source-dirs:       test\n  main-is:              Main.hs\n  other-modules:      Test.Cardano.Slotting.EpochInfo\n  build-depends:        base\n                      , cardano-slotting\n                      , tasty\n                      , tasty-quickcheck\n\n  ghc-options:          -threaded -rtsopts -with-rtsopts=-N\n";
    }