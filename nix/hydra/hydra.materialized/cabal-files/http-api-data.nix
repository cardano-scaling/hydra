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
    flags = { use-text-show = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http-api-data"; version = "0.4.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Nickolay Kudasov <nickolay.kudasov@gmail.com>";
      author = "Nickolay Kudasov <nickolay.kudasov@gmail.com>";
      homepage = "http://github.com/fizruk/http-api-data";
      url = "";
      synopsis = "Converting to/from HTTP API data like URL pieces, headers and query parameters.";
      description = "This package defines typeclasses used for converting Haskell data types to and from HTTP API data.\n\nPlease see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."attoparsec-iso8601" or (errorHandler.buildDepError "attoparsec-iso8601"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.0") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.use-text-show) (hsPkgs."text-show" or (errorHandler.buildDepError "text-show"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-api-data-0.4.3.tar.gz";
      sha256 = "df2747abb477a46c382cd6c2e3a7a2e2f4ba9317fc9de34703e55d4294e02b9c";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.10\nname:            http-api-data\nversion:         0.4.3\nx-revision:      6\n\nsynopsis:        Converting to/from HTTP API data like URL pieces, headers and query parameters.\ncategory:        Web\ndescription:\n  This package defines typeclasses used for converting Haskell data types to and from HTTP API data.\n  .\n  Please see README.md\n\nlicense:         BSD3\nlicense-file:    LICENSE\nauthor:          Nickolay Kudasov <nickolay.kudasov@gmail.com>\nmaintainer:      Nickolay Kudasov <nickolay.kudasov@gmail.com>\nhomepage:        http://github.com/fizruk/http-api-data\nstability:       unstable\nbuild-type:      Simple\n\nextra-source-files:\n  include/overlapping-compat.h\n  test/*.hs\n  CHANGELOG.md\n  README.md\n\ntested-with:\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.4,\n  GHC==8.6.5,\n  GHC==8.8.4,\n  GHC==8.10.7,\n  GHC==9.0.1,\n  GHC==9.2.1\n\nflag use-text-show\n  description: Use text-show library for efficient ToHttpApiData implementations.\n  default: False\n  manual: True\n\nlibrary\n    hs-source-dirs: src/\n    include-dirs:   include/\n\n    -- GHC bundled\n    build-depends:   base                  >= 4.7      && < 4.17\n                   , bytestring            >= 0.10.4.0 && < 0.12\n                   , containers            >= 0.5.5.1  && < 0.7\n                   , text                  >= 1.2.3.0  && < 1.3 || >=2.0 && <2.1\n                   , transformers          >= 0.3      && < 0.7\n\n    -- so Semigroup Builder exists\n    if impl(ghc >= 8.0)\n      build-depends: bytestring >= 0.10.8.1\n\n    -- other-dependencies\n    build-depends:\n                     attoparsec            >= 0.13.2.2 && < 0.15\n                   , attoparsec-iso8601    >= 1.0.2.0  && < 1.1\n                   , base-compat           >= 0.10.5   && < 0.13\n                   , cookie                >= 0.4.3    && < 0.5\n                   , hashable              >= 1.2.7.0  && < 1.5\n                   , http-types            >= 0.12.3   && < 0.13\n                   , tagged                >= 0.8.5    && < 0.9\n                   , time-compat           >= 1.9.5    && < 1.10\n                   , unordered-containers  >= 0.2.10.0 && < 0.3\n                   , uuid-types            >= 1.0.3    && < 1.1\n\n    if !impl(ghc >= 7.10)\n      build-depends:\n        nats >= 1.1.2 && < 1.2,\n        void >= 0.7.3 && < 0.8\n\n    if !impl(ghc >= 8.0)\n      build-depends: semigroups            >= 0.18.5   && < 0.21\n\n    if flag(use-text-show)\n      cpp-options: -DUSE_TEXT_SHOW\n      build-depends: text-show        >= 3.8.2 && <3.10\n\n    exposed-modules:\n      Web.HttpApiData\n      Web.FormUrlEncoded\n      Web.Internal.FormUrlEncoded\n      Web.Internal.HttpApiData\n    ghc-options:     -Wall\n    default-language: Haskell2010\n\ntest-suite spec\n    type:          exitcode-stdio-1.0\n    main-is:       Spec.hs\n    other-modules:\n      Web.Internal.FormUrlEncodedSpec\n      Web.Internal.HttpApiDataSpec\n      Web.Internal.TestInstances\n    hs-source-dirs: test\n    ghc-options:   -Wall\n    default-language: Haskell2010\n    build-tool-depends: hspec-discover:hspec-discover >= 2.7.1 && <2.10\n    -- inherited  depndencies\n    build-depends:\n                     base\n                   , base-compat\n                   , bytestring\n                   , cookie\n                   , http-api-data\n                   , text\n                   , time-compat\n                   , unordered-containers\n                   , uuid-types\n\n    if !impl(ghc >= 7.10)\n      build-depends: nats\n\n    build-depends:   HUnit                >= 1.6.0.0  && <1.7\n                   , hspec                >= 2.7.1    && <2.10\n                   , QuickCheck           >= 2.13.1   && <2.15\n                   , quickcheck-instances >= 0.3.25.2 && <0.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/fizruk/http-api-data\n";
    }