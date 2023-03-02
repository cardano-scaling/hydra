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
      specVersion = "2.4";
      identifier = { name = "monoidal-synchronisation"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "coot@coot.me";
      author = "Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/monoidal-synchronisation-0.1.0.1.tar.gz";
      sha256 = "8399b7c964678cd2053f525c5286c14b60fc551710e81486000b0cfa22f23542";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\nname:               monoidal-synchronisation\nversion:            0.1.0.1\n\n-- A short (one-line) description of the package.\n-- synopsis:\n\n-- A longer description of the package.\n-- description:\n\n-- A URL where users can report bugs.\n-- bug-reports:\n\n-- The license under which the package is released.\nlicense:            Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:             Marcin Szamotulski\nmaintainer:         coot@coot.me\n\n-- A copyright notice.\ncopyright: 2021 Input Output (Hong Kong) Ltd.\n-- category:\nextra-source-files: CHANGELOG.md\n\nlibrary\n  exposed-modules:  Data.Monoid.Synchronisation\n  build-depends:    base >=4.9 && <4.15\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:     -rtsopts\n                   -threaded\n                   -Wall\n                   -Wcompat\n                   -Wincomplete-uni-patterns\n                   -Wincomplete-record-updates\n                   -Wpartial-fields\n                   -Widentities\n                   -Wredundant-constraints\n                   -Wno-unticked-promoted-constructors\n\ntest-suite test\n  type:                exitcode-stdio-1.0\n  main-is:             Main.hs\n  hs-source-dirs:      test\n  other-modules:       Test.Data.Monoid.Synchronisation\n  build-depends:       base\n\n                     , QuickCheck\n                     , tasty\n                     , tasty-quickcheck\n\n                     , io-classes\n                     , io-sim\n                     , monoidal-synchronisation\n  default-language: Haskell2010\n  ghc-options:       -rtsopts\n                     -threaded\n                     -Wall\n                     -Wcompat\n                     -Wincomplete-uni-patterns\n                     -Wincomplete-record-updates\n                     -Wpartial-fields\n                     -Widentities\n                     -Wredundant-constraints\n                     -Wno-unticked-promoted-constructors\n";
    }