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
      identifier = { name = "wl-pprint-annotated"; version = "0.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "2011-2012 Edward A. Kmett, 2000 Daan Leijen";
      maintainer = "Daniel Mendler <mail@daniel-mendler.de>";
      author = "Daniel Mendler <mail@daniel-mendler.de>";
      homepage = "https://github.com/minad/wl-pprint-annotated#readme";
      url = "";
      synopsis = "Pretty printer with annotation support";
      description = "Wadler/Leijen pretty printer with support for annotations and modernized API. Annotations are useful for coloring. See wl-pprint-console.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "wl-pprint" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."wl-pprint-annotated" or (errorHandler.buildDepError "wl-pprint-annotated"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wl-pprint-annotated-0.1.0.1.tar.gz";
      sha256 = "6b662b244b2e318a2923dc7057d707369a29ea4a0e721b4710eac7239cc727af";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.28.2.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n--\r\n-- hash: 456188a057c4e34f6769dab99fc89329114f7149df4168c3485402e7a66bb298\r\n\r\nname:          wl-pprint-annotated\r\ncategory:      Text\r\nversion:       0.1.0.1\r\nx-revision: 1\r\nsynopsis:      Pretty printer with annotation support\r\ncabal-version: >= 1.10\r\nlicense:       BSD3\r\nlicense-file:  LICENSE\r\ntested-with:   GHC == 7.10.3, GHC == 8.0.1, GHC == 8.2.1, GHC == 8.4.3, GHC == 8.6.1\r\nauthor:        Daniel Mendler <mail@daniel-mendler.de>\r\nmaintainer:    Daniel Mendler <mail@daniel-mendler.de>\r\nstability:     experimental\r\nhomepage:      https://github.com/minad/wl-pprint-annotated#readme\r\nbug-reports:   https://github.com/minad/wl-pprint-annotated/issues\r\ncopyright:     2011-2012 Edward A. Kmett, 2000 Daan Leijen\r\ndescription:   Wadler/Leijen pretty printer with support for annotations and modernized API. Annotations are useful for coloring. See wl-pprint-console.\r\nbuild-type:    Simple\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/minad/wl-pprint-annotated\r\n\r\nlibrary\r\n  hs-source-dirs:\r\n    src\r\n  default-extensions: FlexibleInstances TypeSynonymInstances DeriveFunctor DeriveFoldable DeriveTraversable DeriveGeneric DefaultSignatures\r\n  ghc-options: -Wall\r\n  build-depends:\r\n    base >=4.8 && <5,\r\n    containers >=0.4 && <0.7,\r\n    deepseq >=1.4 && <1.6,\r\n    text >=0.11 && <2.1\r\n  if impl(ghc < 8.0)\r\n    build-depends:\r\n      semigroups >=0.9 && <1\r\n  exposed-modules:\r\n    Text.PrettyPrint.Annotated.WL\r\n  other-modules:\r\n    Paths_wl_pprint_annotated\r\n  default-language: Haskell2010\r\n\r\ntest-suite wl-pprint\r\n  type: exitcode-stdio-1.0\r\n  main-is: WLPPrintTests.hs\r\n  other-modules:\r\n    Paths_wl_pprint_annotated\r\n  hs-source-dirs:\r\n    test\r\n  default-extensions: FlexibleInstances TypeSynonymInstances DeriveFunctor DeriveFoldable DeriveTraversable DeriveGeneric DefaultSignatures\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  build-depends:\r\n    base >=4.8 && <5,\r\n    containers >=0.4 && <0.7,\r\n    deepseq >=1.4 && <1.6,\r\n    tasty,\r\n    tasty-hunit,\r\n    text >=0.11 && <2.1,\r\n    wl-pprint-annotated\r\n  if impl(ghc < 8.0)\r\n    build-depends:\r\n      semigroups >=0.9 && <1\r\n  default-language: Haskell2010\r\n";
    }