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
      specVersion = "1.10";
      identifier = { name = "string-conversions"; version = "0.4.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "soenkehahn@gmail.com";
      author = "Sönke Hahn";
      homepage = "https://github.com/soenkehahn/string-conversions#readme";
      url = "";
      synopsis = "Simplifies dealing with different types for strings";
      description = "Provides a simple type class for converting values of different string types into values of other string types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/string-conversions-0.4.0.1.tar.gz";
      sha256 = "46bcce6d9ce62c558b7658a75d9c6a62f7259d6b0473d011d8078234ad6a1994";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.15.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:                string-conversions\nversion:             0.4.0.1\nsynopsis:            Simplifies dealing with different types for strings\ndescription:         Provides a simple type class for converting values of different string types into values of other string types.\nlicense:             BSD3\nlicense-file:        LICENSE\ntested-with:         GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.1, GHC == 8.0.1\nauthor:              Sönke Hahn\nmaintainer:          soenkehahn@gmail.com\ncategory:            Data\nhomepage:            https://github.com/soenkehahn/string-conversions#readme\nbug-reports:         https://github.com/soenkehahn/string-conversions/issues\nbuild-type:          Simple\ncabal-version:       >= 1.10\n\nsource-repository head\n  type: git\n  location: https://github.com/soenkehahn/string-conversions\n\nlibrary\n  hs-source-dirs:\n    src\n  default-language: Haskell2010\n  build-depends:\n    base == 4.*,\n    bytestring >= 0.9,\n    text >= 0.11,\n    utf8-string >= 0.3\n  exposed-modules:\n    Data.String.Conversions\n    Data.String.Conversions.Monomorphic\n  ghc-options: -Wall\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n    test,\n    src\n  build-depends:\n    base == 4.*,\n    bytestring >= 0.9,\n    text >= 0.11,\n    utf8-string >= 0.3,\n    hspec,\n    quickcheck-instances,\n    deepseq,\n    QuickCheck\n  other-modules:\n    Data.String.ConversionsSpec\n    Data.String.Conversions\n    Data.String.Conversions.Monomorphic\n  default-language: Haskell2010\n";
    }