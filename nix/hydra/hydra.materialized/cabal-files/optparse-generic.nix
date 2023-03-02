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
      identifier = { name = "optparse-generic"; version = "1.4.8"; };
      license = "BSD-3-Clause";
      copyright = "2016 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Auto-generate a command-line parser for your datatype";
      description = "This library auto-generates an @optparse-applicative@-compatible\n@Parser@ from any data type that derives the @Generic@ interface.\n\nSee the documentation in \"Options.Generic\" for an example of how to use\nthis library";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."Only" or (errorHandler.buildDepError "Only"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.8") [
          (hsPkgs."singletons" or (errorHandler.buildDepError "singletons"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."th-desugar" or (errorHandler.buildDepError "th-desugar"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optparse-generic-1.4.8.tar.gz";
      sha256 = "aa45862d5c59b21403fbd6f6771fa8047b7c82043efbd38bbe59e08ae5961a81";
      });
    }) // {
    package-description-override = "Name: optparse-generic\r\nVersion: 1.4.8\r\nx-revision: 1\r\nCabal-Version: >=1.10\r\nBuild-Type: Simple\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2016 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nTested-With: GHC == 7.8.4, GHC == 7.10.2, GHC == 8.0.1\r\nBug-Reports: https://github.com/Gabriella439/Haskell-Optparse-Generic-Library/issues\r\nSynopsis: Auto-generate a command-line parser for your datatype\r\nDescription: This library auto-generates an @optparse-applicative@-compatible\r\n    @Parser@ from any data type that derives the @Generic@ interface.\r\n    .\r\n    See the documentation in \"Options.Generic\" for an example of how to use\r\n    this library\r\nCategory: System\r\nExtra-Source-Files: CHANGELOG.md\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-Optparse-Generic-Library\r\n\r\nLibrary\r\n    Hs-Source-Dirs: src\r\n    Build-Depends:\r\n        base                 >= 4.7      && < 5   ,\r\n        system-filepath      >= 0.3.1    && < 0.5 ,\r\n        text                                < 2.1 ,\r\n        transformers         >= 0.2.0.0  && < 0.7 ,\r\n        transformers-compat  >= 0.3      && < 0.8 ,\r\n        Only                                < 0.2 ,\r\n        optparse-applicative >= 0.16.0.0 && < 0.18,\r\n        time                 >= 1.5      && < 1.13,\r\n        void                                < 0.8 ,\r\n        bytestring                          < 0.12\r\n    \r\n    if impl(ghc < 8.0)\r\n        Build-Depends:\r\n            semigroups           >= 0.5.0    && < 0.20\r\n\r\n    if impl(ghc < 7.8)\r\n        Build-Depends:\r\n            singletons       >= 0.10.0  && < 1.0 ,\r\n            tagged           >= 0.8.3   && < 0.9 ,\r\n            th-desugar                     < 1.5.1\r\n    Exposed-Modules: Options.Generic\r\n    GHC-Options: -Wall\r\n    Default-Language: Haskell2010\r\n";
    }