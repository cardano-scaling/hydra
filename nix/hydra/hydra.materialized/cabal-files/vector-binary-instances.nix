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
      specVersion = "3.0";
      identifier = { name = "vector-binary-instances"; version = "0.2.5.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "dons00@gmail.com, bos@serpentine.com, Ben Gamari <ben@smart-cactus.org>";
      author = "Don Stewart";
      homepage = "https://github.com/bos/vector-binary-instances";
      url = "";
      synopsis = "Instances of Data.Binary for vector";
      description = "Instances for Binary for the types defined in the vector package,\nmaking it easy to serialize vectors to and from disk. We use the\ngeneric interface to vectors, so all vector types are supported.\nSpecific instances are provided for unboxed, boxed and storable\nvectors.\n.\nTo serialize a vector:\n.\n> *Data.Vector.Binary> let v = Data.Vector.fromList [1..10]\n> *Data.Vector.Binary> v\n> fromList [1,2,3,4,5,6,7,8,9,10] :: Data.Vector.Vector\n> *Data.Vector.Binary> encode v\n> Chunk \"\\NUL\\NUL\\NUL\\NUL\\NUL...\\NUL\\NUL\\NUL\\t\\NUL\\NUL\\NUL\\NUL\\n\" Empty\n.\nWhich you can in turn compress before writing to disk:\n.\n> compress . encode $ v\n> Chunk \"\\US\\139\\b\\NUL\\NUL\\N...\\229\\240,\\254:\\NUL\\NUL\\NUL\" Empty\n.\nTry the cereal-vector package if you are looking for Data.Serialize\ninstances.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vector-binary-instances" or (errorHandler.buildDepError "vector-binary-instances"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vector-binary-instances" or (errorHandler.buildDepError "vector-binary-instances"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-binary-instances-0.2.5.2.tar.gz";
      sha256 = "b72e3b2109a02c75cb8f07ef0aabba0dba6ec0148e21321a0a2b2197c9a2f54d";
      });
    }) // {
    package-description-override = "Cabal-version:       3.0\r\nName:                vector-binary-instances\r\nVersion:             0.2.5.2\r\nx-revision: 2\r\nSynopsis:            Instances of Data.Binary for vector\r\nDescription:\r\n   Instances for Binary for the types defined in the vector package,\r\n   making it easy to serialize vectors to and from disk. We use the\r\n   generic interface to vectors, so all vector types are supported.\r\n   Specific instances are provided for unboxed, boxed and storable\r\n   vectors.\r\n   .\r\n   To serialize a vector:\r\n   .\r\n   > *Data.Vector.Binary> let v = Data.Vector.fromList [1..10]\r\n   > *Data.Vector.Binary> v\r\n   > fromList [1,2,3,4,5,6,7,8,9,10] :: Data.Vector.Vector\r\n   > *Data.Vector.Binary> encode v\r\n   > Chunk \"\\NUL\\NUL\\NUL\\NUL\\NUL...\\NUL\\NUL\\NUL\\t\\NUL\\NUL\\NUL\\NUL\\n\" Empty\r\n   .\r\n   Which you can in turn compress before writing to disk:\r\n   .\r\n   > compress . encode $ v\r\n   > Chunk \"\\US\\139\\b\\NUL\\NUL\\N...\\229\\240,\\254:\\NUL\\NUL\\NUL\" Empty\r\n   .\r\n   Try the cereal-vector package if you are looking for Data.Serialize\r\n   instances.\r\n\r\n-- URL for the project homepage or repository.\r\nHomepage:            https://github.com/bos/vector-binary-instances\r\nbug-reports:         https://github.com/bos/vector-binary-instances/issues\r\nLicense:             BSD-3-Clause\r\nLicense-file:        LICENSE\r\nAuthor:              Don Stewart\r\nMaintainer:          dons00@gmail.com, bos@serpentine.com, Ben Gamari <ben@smart-cactus.org>\r\nTested-With:         GHC==8.10.1, GHC==8.8.3, GHC==8.6.5, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\r\nStability:           Experimental\r\nCategory:            Data\r\nBuild-type:          Simple\r\n\r\nLibrary\r\n  Ghc-options: -Wall\r\n  -- Modules exported by the library.\r\n  Exposed-modules:\r\n    Data.Vector.Binary\r\n\r\n  -- Packages needed in order to build this package.\r\n  Build-depends:\r\n    base > 3 && < 4.17,\r\n    vector >= 0.6 && < 0.14,\r\n    binary >= 0.5 && < 0.11\r\n  Default-Language: Haskell2010\r\n\r\nBenchmark benchmarks\r\n  Type:           exitcode-stdio-1.0\r\n  Main-is:        Benchmarks.hs\r\n  Build-depends:\r\n    base >= 4.7,\r\n    vector-binary-instances,\r\n    vector,\r\n    bytestring,\r\n    binary,\r\n    tasty-bench,\r\n    deepseq\r\n  hs-source-dirs: benchmarks\r\n  Default-Language: Haskell2010\r\n\r\nTest-Suite tests\r\n  Type:           exitcode-stdio-1.0\r\n  Main-is:        test/Main.hs\r\n  Build-depends:\r\n    base,\r\n    vector-binary-instances,\r\n    vector,\r\n    binary,\r\n    tasty,\r\n    tasty-quickcheck\r\n  Default-Language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/bos/vector-binary-instances\r\n";
    }