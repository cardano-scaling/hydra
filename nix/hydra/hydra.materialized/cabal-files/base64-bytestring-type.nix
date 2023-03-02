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
    flags = { cereal = true; serialise = true; http-api-data = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "base64-bytestring-type"; version = "1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/base64-bytestring-type#readme";
      url = "";
      synopsis = "A newtype around ByteString, for base64 encoding";
      description = "A newtype around ByteString, for base64 encoding.\nStrict and lazy, normal and url alphabet variants.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.cereal) (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))) ++ (pkgs.lib).optional (flags.serialise) (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))) ++ (pkgs.lib).optional (flags.http-api-data) (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = (([
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ] ++ (pkgs.lib).optional (flags.cereal) (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))) ++ (pkgs.lib).optional (flags.serialise) (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))) ++ (pkgs.lib).optional (flags.http-api-data) (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base64-bytestring-type-1.0.1.tar.gz";
      sha256 = "f607d07c4aab227b4536c495fa7c07b35ddc9c2c013d385c16c02f236526780e";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               base64-bytestring-type\nversion:            1.0.1\nx-revision:         15\nsynopsis:           A newtype around ByteString, for base64 encoding\ndescription:\n  A newtype around ByteString, for base64 encoding.\n  Strict and lazy, normal and url alphabet variants.\n\ncategory:           Data\nhomepage:           https://github.com/phadej/base64-bytestring-type#readme\nbug-reports:        https://github.com/phadej/base64-bytestring-type/issues\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\nbuild-type:         Simple\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/futurice/haskell-base64-bytestring-type\n\nflag cereal\n  description: Instances for @Serialize@ from @cereal@ package\n  manual:      True\n  default:     True\n\nflag serialise\n  description: Instances for @Serialise@ from @serialise@ package\n  manual:      True\n  default:     True\n\nflag http-api-data\n  description:\n    Instances for @To/FromHttpApiData@ from @http-api-data@ package\n\n  manual:      True\n  default:     True\n\nlibrary\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n\n  -- boot libraries\n  -- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory\n  build-depends:\n      base        >=4.7.0.0  && <4.18\n    , binary      >=0.7.1.0  && <0.10\n    , bytestring  >=0.10.4.0 && <0.12\n    , deepseq     >=1.3.0.2  && <1.5\n    , text        >=1.2.3.0  && <1.3 || >=2.0 && <2.1\n\n  -- other dependencies:\n  build-depends:\n      aeson              >=1.2.3.0 && <1.6 || >=2.0 && <2.2\n    , base-compat        >=0.9.3   && <0.13\n    , base64-bytestring  >=1.0.0.1 && <1.3\n    , hashable           >=1.2.6.1 && <1.5\n    , QuickCheck         >=2.11.3  && <2.15\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.5 && <0.21\n\n  if flag(cereal)\n    build-depends: cereal >=0.5.5.0 && <0.6\n\n  if flag(serialise)\n    build-depends: serialise >=0.2.1.0 && <0.3\n\n  if flag(http-api-data)\n    build-depends: http-api-data >=0.4 && <0.6\n\n  exposed-modules:\n    Data.ByteString.Base64.Lazy.Type\n    Data.ByteString.Base64.Type\n    Data.ByteString.Base64.URL.Lazy.Type\n    Data.ByteString.Base64.URL.Type\n\n  default-language: Haskell2010\n\ntest-suite tests\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   test\n  ghc-options:      -Wall\n\n  -- dependencies with bounds inherited from library:\n  build-depends:\n      aeson\n    , base\n    , base64-bytestring-type\n    , binary\n    , bytestring\n\n  if flag(cereal)\n    build-depends: cereal\n\n  if flag(serialise)\n    build-depends: serialise\n\n  if flag(http-api-data)\n    build-depends: http-api-data\n\n  -- other dependencies\n  build-depends:\n      tasty             >=1.2.1 && <1.5\n    , tasty-quickcheck  >=0.10  && <0.11\n";
    }