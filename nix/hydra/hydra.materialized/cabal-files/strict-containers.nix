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
      identifier = { name = "strict-containers"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Various strict container types";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/strict-containers-0.1.0.0.tar.gz";
      sha256 = "d6e633ef6f9c0a919a05e1b7b9b8bb6e252a2a9e3d057eee48241188b0726203";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                strict-containers\nversion:             0.1.0.0\nsynopsis:            Various strict container types\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n  if (!flag(development))\n    ghc-options:\n      -Werror\n  exposed-modules:      Data.FingerTree.Strict\n                        Data.Maybe.Strict\n                        Data.Sequence.Strict\n                        Data.Unit.Strict\n  build-depends:        aeson\n                      , base >= 4.14\n                      , cardano-binary\n                      , cborg\n                      , containers\n                      , data-default-class\n                      , deepseq\n                      , fingertree\n                      , nothunks\n                      , serialise\n";
    }