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
      specVersion = "1.12";
      identifier = { name = "int-cast"; version = "0.2.0.0.0.0.0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "https://github.com/hvr/int-cast";
      url = "";
      synopsis = "Checked conversions between integral types";
      description = "Provides statically or dynamically checked conversions between integral types. See documentation in \"Data.IntCast\" for more details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
        buildable = true;
        };
      tests = {
        "int-cast-test" = {
          depends = [
            (hsPkgs."int-cast" or (errorHandler.buildDepError "int-cast"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/int-cast-0.2.0.0.0.0.0.0.1.tar.gz";
      sha256 = "8a32f02ba18e3c087a9d5edcbf5e86d1967737fe442918886c0823a64c9722b6";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               int-cast\nversion:            0.2.0.0.0.0.0.0.1\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         hvr@gnu.org\nauthor:             Herbert Valerio Riedel\nhomepage:           https://github.com/hvr/int-cast\nbug-reports:        https://github.com/hvr/int-cast/issues\nsynopsis:           Checked conversions between integral types\ndescription:\n    Provides statically or dynamically checked conversions between integral types. See documentation in \"Data.IntCast\" for more details.\n\ncategory:           Data\nbuild-type:         Simple\nextra-source-files: changelog.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/hvr/int-cast.git\n\nlibrary\n    exposed-modules:  Data.IntCast\n    default-language: Haskell2010\n    other-extensions:\n        CPP DataKinds TypeFamilies TypeOperators UndecidableInstances\n\n    ghc-options:      -Wall\n    build-depends:    base >=4.7 && <4.17\n\n    if !impl(ghc >=7.10)\n        build-depends: nats >=0.1 && <1.1\n\ntest-suite int-cast-test\n    type:             exitcode-stdio-1.0\n    main-is:          Suite.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        int-cast,\n        base,\n        QuickCheck >=2.14 && <2.15,\n        test-framework >=0.8 && <0.9,\n        test-framework-quickcheck2 >=0.3 && <0.4\n\n    if !impl(ghc >=7.10)\n        build-depends: nats\n";
    }