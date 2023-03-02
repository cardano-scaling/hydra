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
      identifier = { name = "base-deriving-via"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A general hook newtype for use with deriving via";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/base-deriving-via-0.1.0.0.tar.gz";
      sha256 = "aaa0654563aead7114cced80a3a807cf72de630dbba61d4d65d5fc7829e9ae4e";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\n\nname:                base-deriving-via\nversion:             0.1.0.0\nsynopsis:            A general hook newtype for use with deriving via\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           IOHK\nbuild-type:          Simple\n\nflag development\n    description: Disable `-Werror`\n    default: False\n    manual: True\n\nlibrary\n  default-language:     Haskell2010\n  hs-source-dirs:       src\n\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-uni-patterns\n    -Wincomplete-record-updates\n    -Wpartial-fields\n    -Widentities\n    -Wredundant-constraints\n    -Wmissing-export-lists\n  if (!flag(development))\n    ghc-options:\n      -Werror\n\n  exposed-modules:\n                        Data.DerivingVia\n                        Data.DerivingVia.GHC.Generics.Monoid\n                        Data.DerivingVia.GHC.Generics.Semigroup\n\n  build-depends:        base >= 4.14\n";
    }