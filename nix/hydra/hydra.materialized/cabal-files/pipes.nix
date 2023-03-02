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
      identifier = { name = "pipes"; version = "4.3.16"; };
      license = "BSD-3-Clause";
      copyright = "2012-2016 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Compositional pipelines";
      description = "`pipes` is a clean and powerful stream processing library that lets you build\nand connect reusable streaming components\n\nAdvantages over traditional streaming libraries:\n\n* /Concise API/: Use simple commands like 'for', ('>->'), 'await', and 'yield'\n\n* /Blazing fast/: Implementation tuned for speed, including shortcut fusion\n\n* /Lightweight Dependency/: @pipes@ is small and compiles very rapidly,\nincluding dependencies\n\n* /Elegant semantics/: Use practical category theory\n\n* /ListT/: Correct implementation of 'ListT' that interconverts with pipes\n\n* /Bidirectionality/: Implement duplex channels\n\n* /Extensive Documentation/: Second to none!\n\nImport \"Pipes\" to use the library.\n\nRead \"Pipes.Tutorial\" for an extensive tutorial.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.0") [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "prelude-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            ];
          buildable = true;
          };
        "lift-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pipes-4.3.16.tar.gz";
      sha256 = "f4e16ecf010fd681a56e6216ab1bd429f3c9bc962ec032e32cfd23e374e97498";
      });
    }) // {
    package-description-override = "Name: pipes\r\nVersion: 4.3.16\r\nx-revision: 6\r\nCabal-Version: >= 1.10\r\nBuild-Type: Simple\r\nTested-With: GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.1\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2012-2016 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nBug-Reports: https://github.com/Gabriella439/Haskell-Pipes-Library/issues\r\nSynopsis: Compositional pipelines\r\nDescription:\r\n  `pipes` is a clean and powerful stream processing library that lets you build\r\n  and connect reusable streaming components\r\n  .\r\n  Advantages over traditional streaming libraries:\r\n  .\r\n  * /Concise API/: Use simple commands like 'for', ('>->'), 'await', and 'yield'\r\n  .\r\n  * /Blazing fast/: Implementation tuned for speed, including shortcut fusion\r\n  .\r\n  * /Lightweight Dependency/: @pipes@ is small and compiles very rapidly,\r\n    including dependencies\r\n  .\r\n  * /Elegant semantics/: Use practical category theory\r\n  .\r\n  * /ListT/: Correct implementation of 'ListT' that interconverts with pipes\r\n  .\r\n  * /Bidirectionality/: Implement duplex channels\r\n  .\r\n  * /Extensive Documentation/: Second to none!\r\n  .\r\n  Import \"Pipes\" to use the library.\r\n  .\r\n  Read \"Pipes.Tutorial\" for an extensive tutorial.\r\nCategory: Control, Pipes\r\nExtra-Source-Files:\r\n    CHANGELOG.md\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-Pipes-Library\r\n\r\nLibrary\r\n    Default-Language: Haskell2010\r\n\r\n    HS-Source-Dirs: src\r\n    Build-Depends:\r\n        base         >= 4.8     && < 5   ,\r\n        transformers >= 0.2.0.0 && < 0.7 ,\r\n        exceptions   >= 0.4     && < 0.11,\r\n        mmorph       >= 1.0.4   && < 1.3 ,\r\n        mtl          >= 2.2.1   && < 2.4 ,\r\n        void         >= 0.4     && < 0.8\r\n\r\n    if impl(ghc < 8.0)\r\n        Build-depends:\r\n            fail       == 4.9.*         ,\r\n            semigroups >= 0.17 && < 0.20\r\n\r\n    Exposed-Modules:\r\n        Pipes,\r\n        Pipes.Core,\r\n        Pipes.Internal,\r\n        Pipes.Lift,\r\n        Pipes.Prelude,\r\n        Pipes.Tutorial\r\n    GHC-Options: -O2 -Wall\r\n\r\nBenchmark prelude-benchmarks\r\n    Default-Language: Haskell2010\r\n    Type:             exitcode-stdio-1.0\r\n    HS-Source-Dirs:   benchmarks\r\n    Main-Is:          PreludeBench.hs\r\n    Other-Modules:    Common\r\n    GHC-Options:     -O2 -Wall -rtsopts -fno-warn-unused-do-bind\r\n\r\n    Build-Depends:\r\n        base      >= 4.4     && < 5  ,\r\n        criterion >= 1.1.1.0 && < 1.7,\r\n        optparse-applicative >= 0.12 && < 0.18,\r\n        mtl       >= 2.1     && < 2.4,\r\n        pipes\r\n\r\ntest-suite tests\r\n    Default-Language: Haskell2010\r\n    Type:             exitcode-stdio-1.0\r\n    HS-Source-Dirs:   tests\r\n    Main-Is:          Main.hs\r\n    GHC-Options:      -Wall -rtsopts -fno-warn-missing-signatures -fno-enable-rewrite-rules\r\n\r\n    Build-Depends:\r\n        base                       >= 4.4     && < 5   ,\r\n        pipes                                          ,\r\n        QuickCheck                 >= 2.4     && < 3   ,\r\n        mtl                        >= 2.1     && < 2.4 ,\r\n        test-framework             >= 0.4     && < 1   ,\r\n        test-framework-quickcheck2 >= 0.2.0   && < 0.4 ,\r\n        transformers               >= 0.2.0.0 && < 0.7\r\n\r\nBenchmark lift-benchmarks\r\n    Default-Language: Haskell2010\r\n    Type:             exitcode-stdio-1.0\r\n    HS-Source-Dirs:   benchmarks\r\n    Main-Is:          LiftBench.hs\r\n    Other-Modules:    Common\r\n    GHC-Options:     -O2 -Wall -rtsopts -fno-warn-unused-do-bind\r\n\r\n    Build-Depends:\r\n        base                 >= 4.4     && < 5   ,\r\n        criterion            >= 1.1.1.0 && < 1.7 ,\r\n        optparse-applicative                     ,\r\n        mtl                  >= 2.1     && < 2.4 ,\r\n        pipes                                    ,\r\n        transformers         >= 0.2.0.0 && < 0.7\r\n";
    }