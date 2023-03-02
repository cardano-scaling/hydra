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
      specVersion = "1.10";
      identifier = { name = "orphans-deriving-via"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Orphan instances for the base-deriving-via hooks";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/orphans-deriving-via-0.1.0.0.tar.gz";
      sha256 = "809569cbba0d8b4bf64a585326e388f2a0c4476bf4306d5a9ede2000ce44a619";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                orphans-deriving-via\nversion:             0.1.0.0\nsynopsis:            Orphan instances for the base-deriving-via hooks\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n  if (!flag(development))\n    ghc-options:\n      -Werror\n\n  exposed-modules:\n                        Data.DerivingVia.DeepSeq\n                        Data.DerivingVia.NoThunks\n\n  build-depends:        base >= 4.14\n                      , base-deriving-via\n                      , deepseq\n                      , nothunks\n";
    }