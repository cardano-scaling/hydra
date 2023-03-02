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
      identifier = { name = "hkd"; version = "0.2"; };
      license = "(BSD-2-Clause OR Apache-2.0)";
      copyright = "Copyright (c) 2019 Edward Kmett, 2019 Oleg Grenrus";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Edward Kmett <ekmett@gmail.com>";
      homepage = "https://github.com/ekmett/codex/tree/master/hkd#readme";
      url = "";
      synopsis = "\"higher-kinded data\"";
      description = "\"Higher-kinded data\" utilities, e.g.\n\n@\nclass FFunctor t where\n\\    ffmap :: (f ~> g) -> t f -> t g\n@\n\nand other classes and types.\n\n/Note:/ this package is experimental.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      tests = {
        "example-np" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hkd" or (errorHandler.buildDepError "hkd"))
            ];
          buildable = true;
          };
        "example-record" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hkd" or (errorHandler.buildDepError "hkd"))
            (hsPkgs."some" or (errorHandler.buildDepError "some"))
            ];
          buildable = true;
          };
        "example-issue-12" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hkd" or (errorHandler.buildDepError "hkd"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hkd-0.2.tar.gz";
      sha256 = "1c3db4f8550b270e69d0a113993bba8761129e8604e8442fc6a21e6f6ce100eb";
      });
    }) // {
    package-description-override = "cabal-version:   2.2\nname:            hkd\nversion:         0.2\nsynopsis:        \"higher-kinded data\"\ndescription:\n  \"Higher-kinded data\" utilities, e.g.\n  .\n  @\n  class FFunctor t where\n  \\    ffmap :: (f ~> g) -> t f -> t g\n  @\n  .\n  and other classes and types.\n  .\n  /Note:/ this package is experimental.\n\nhomepage:        https://github.com/ekmett/codex/tree/master/hkd#readme\nlicense:         (BSD-2-Clause OR Apache-2.0)\nlicense-file:    LICENSE.md\nauthor:          Edward Kmett <ekmett@gmail.com>\nmaintainer:      Oleg Grenrus <oleg.grenrus@iki.fi>\ncopyright:       Copyright (c) 2019 Edward Kmett, 2019 Oleg Grenrus\ncategory:        Data Structures\nbuild-type:      Simple\nextra-doc-files:\n  README.md\n  CHANGELOG.md\n\ntested-with:\n  GHC ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/hkd\n  subdir:   hkd\n\nlibrary\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  exposed-modules:  Data.HKD\n  other-modules:    Data.Functor.Confusing\n\n  if impl(ghc >=8.0)\n    ghc-options: -Wno-trustworthy-safe\n\n  if impl(ghc >=8.4)\n    ghc-options:\n      -Wincomplete-uni-patterns -Wincomplete-record-updates\n      -Wredundant-constraints -Widentities -Wmissing-export-lists\n\n  build-depends:\n    , base  >=4.6     && <4.18\n    , some  ^>=1.0.0.3\n\n  if !impl(ghc >=7.10)\n    build-depends: transformers >=0.3 && <0.6\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.5 && <1\n\n  if !impl(ghc >=7.8)\n    build-depends: tagged >=0.8.5 && <1\n\ntest-suite example-np\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   example\n  main-is:          NP.hs\n  build-depends:\n    , base\n    , hkd\n\ntest-suite example-record\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   example\n  main-is:          Record.hs\n\n  -- build-depends: dump-core\n  -- ghc-options:   -fplugin=DumpCore\n\n  build-depends:\n    , base\n    , hkd\n    , some\n\ntest-suite example-issue-12\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   example\n  main-is:          example-issue-12.hs \n\n  build-depends:\n    , base\n    , hkd\n";
    }