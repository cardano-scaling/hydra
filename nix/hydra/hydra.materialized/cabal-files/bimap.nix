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
      specVersion = "1.8";
      identifier = { name = "bimap"; version = "0.4.0"; };
      license = "BSD-3-Clause";
      copyright = "Stuart Cook and contributors 2008, Joel Williamson 2015";
      maintainer = "Joel Williamson <joel@joelwilliamson.ca>";
      author = "Stuart Cook and contributors 2008, Joel Williamson 2015";
      homepage = "https://github.com/joelwilliamson/bimap";
      url = "";
      synopsis = "Bidirectional mapping between two key types";
      description = "A data structure representing a bidirectional mapping between two\nkey types. Each value in the bimap is associated with exactly one\nvalue of the opposite type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6.1") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6.1") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bimap-0.4.0.tar.gz";
      sha256 = "d1a39686abbfed5864a8fb778d2244825b6eac977e130e7c1212e6d3a68f249d";
      });
    }) // {
    package-description-override = "cabal-version:       >= 1.8\r\nname:                bimap\r\nversion:             0.4.0\r\nx-revision: 1\r\nsynopsis:            Bidirectional mapping between two key types\r\ndescription:\r\n  A data structure representing a bidirectional mapping between two\r\n  key types. Each value in the bimap is associated with exactly one\r\n  value of the opposite type.\r\ncategory:            Data\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\ncopyright:           Stuart Cook and contributors 2008, Joel Williamson 2015\r\nauthor:              Stuart Cook and contributors 2008, Joel Williamson 2015\r\nmaintainer:          Joel Williamson <joel@joelwilliamson.ca>\r\nhomepage:            https://github.com/joelwilliamson/bimap\r\nbuild-type:          Simple\r\ntested-with:         GHC <= 8.6.4 && >= 7.0\r\nextra-source-files:\r\n    HISTORY\r\n\r\nLibrary\r\n  build-depends:       base >= 4 && <5, containers, deepseq, exceptions\r\n  if impl(ghc < 7.6.1)\r\n    build-depends: ghc-prim\r\n  extensions:          DeriveDataTypeable\r\n  ghc-options:         -Wall\r\n  exposed-modules:\r\n      Data.Bimap\r\n\r\ntest-suite tests\r\n    type:            exitcode-stdio-1.0\r\n    main-is:         Test/RunTests.hs\r\n    other-modules:   Test.Tests\r\n                     Test.Util\r\n    build-depends:   base >= 4 && < 5,\r\n                     containers,\r\n                     deepseq,\r\n                     exceptions,\r\n                     QuickCheck >= 2 && < 3,\r\n                     template-haskell >= 2 && < 3\r\n    if impl(ghc < 7.6.1)\r\n      build-depends: ghc-prim\r\n  extensions:        DeriveDataTypeable\r\n  other-extensions:  DeriveGeneric\r\n\r\nsource-repository head\r\n    type:         git\r\n    location:     https://github.com/joelwilliamson/bimap.git\r\n";
    }