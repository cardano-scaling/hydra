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
      identifier = { name = "foldl"; version = "1.4.12"; };
      license = "BSD-3-Clause";
      copyright = "2013 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Composable, streaming, and efficient left folds";
      description = "This library provides strict left folds that stream in constant\nmemory, and you can combine folds using @Applicative@ style to derive new\nfolds.  Derived folds still traverse the container just once and are often as\nefficient as hand-written folds.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "Foldl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            ];
          buildable = true;
          };
        "Scanl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/foldl-1.4.12.tar.gz";
      sha256 = "4f59360d96fb9ff10861944dd8a89b2448ea2b7dedc376546f4de80125f5c47d";
      });
    }) // {
    package-description-override = "Name: foldl\r\nVersion: 1.4.12\r\nx-revision: 5\r\nCabal-Version: >=1.10\r\nBuild-Type: Simple\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2013 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nBug-Reports: https://github.com/Gabriella439/Haskell-Foldl-Library/issues\r\nSynopsis: Composable, streaming, and efficient left folds\r\nDescription: This library provides strict left folds that stream in constant\r\n  memory, and you can combine folds using @Applicative@ style to derive new\r\n  folds.  Derived folds still traverse the container just once and are often as\r\n  efficient as hand-written folds.\r\nCategory: Control\r\nExtra-Source-Files:\r\n    CHANGELOG.md\r\n    README.md\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-Foldl-Library\r\n\r\nLibrary\r\n    HS-Source-Dirs: src\r\n    Build-Depends:\r\n        base         >= 4.8      && < 5   ,\r\n        bytestring   >= 0.9.2.1  && < 0.12,\r\n        random       >= 1.2      && < 1.3 ,\r\n        primitive                   < 0.8 ,\r\n        text         >= 0.11.2.0 && < 2.1 ,\r\n        transformers >= 0.2.0.0  && < 0.7 ,\r\n        vector       >= 0.7      && < 0.14,\r\n        containers   >= 0.5.0.0  && < 0.7 ,\r\n        unordered-containers        < 0.3 ,\r\n        hashable                    < 1.5 ,\r\n        contravariant               < 1.6 ,\r\n        profunctors                 < 5.7 ,\r\n        semigroupoids >= 1.0     && < 5.4 ,\r\n        comonad      >= 4.0      && < 6\r\n    if impl(ghc < 8.0)\r\n        Build-Depends:\r\n            semigroups   >= 0.17 && < 1.20\r\n    Exposed-Modules:\r\n        Control.Foldl,\r\n        Control.Foldl.ByteString,\r\n        Control.Foldl.Text,\r\n        Control.Scanl\r\n    Other-Modules:\r\n        Control.Foldl.Optics\r\n        Control.Foldl.Internal\r\n        Control.Foldl.Util.Vector\r\n        Control.Foldl.Util.MVector\r\n    GHC-Options: -O2 -Wall\r\n    Default-Language: Haskell2010\r\n\r\nBenchmark Foldl\r\n    Type: exitcode-stdio-1.0\r\n    HS-Source-Dirs: bench\r\n    Main-Is: Foldl.hs\r\n    Build-Depends:\r\n        base,\r\n        criterion,\r\n        foldl\r\n    GHC-Options: -O2 -Wall -rtsopts -with-rtsopts=-T\r\n    Default-Language: Haskell2010\r\n\r\nBenchmark Scanl\r\n    Type: exitcode-stdio-1.0\r\n    HS-Source-Dirs: bench\r\n    Main-Is: Scanl.hs\r\n    Build-Depends:\r\n        base,\r\n        criterion,\r\n        foldl\r\n    GHC-Options: -O2 -Wall -rtsopts -with-rtsopts=-T\r\n    Default-Language: Haskell2010\r\n\r\nTest-Suite doctest\r\n    Type: exitcode-stdio-1.0\r\n    HS-Source-Dirs: test\r\n    Main-Is: doctest.hs\r\n    Build-Depends:\r\n        base,\r\n        doctest >= 0.16\r\n    GHC-Options: -threaded\r\n    Default-Language: Haskell2010\r\n";
    }