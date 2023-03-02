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
      specVersion = "2.2";
      identifier = { name = "data-clist"; version = "0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jeremy Huffman <jeremy@jeremyhuffman.com>, John Van Enk <vanenkj@gmail.com>";
      author = "John Van Enk <vanenkj@gmail.com>";
      homepage = "https://github.com/sw17ch/data-clist";
      url = "";
      synopsis = "Simple functional ring type.";
      description = "Simple functional bidirectional ring type.\nGiven that the ring terminiology clashes with certain\nmathematical branches, we're using the term CList or\nCircularList instead.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-clist" or (errorHandler.buildDepError "data-clist"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-clist-0.2.tar.gz";
      sha256 = "1c3a1ebd71e8f6fe30afdb3797c4852db7fb0b4e3c145fc53e8282eb4303b212";
      });
    }) // {
    package-description-override = "Cabal-Version: 2.2\nName: data-clist\nSynopsis: Simple functional ring type.\nDescription: Simple functional bidirectional ring type.\n\n             Given that the ring terminiology clashes with certain\n             mathematical branches, we're using the term CList or\n             CircularList instead.\nVersion: 0.2\nLicense: BSD-3-Clause\nLicense-File: LICENSE\nAuthor: John Van Enk <vanenkj@gmail.com>\nMaintainer: Jeremy Huffman <jeremy@jeremyhuffman.com>, John Van Enk <vanenkj@gmail.com>\nStability: experimental\nCategory: Data Structures\nBuild-Type: Simple\nHomepage: https://github.com/sw17ch/data-clist\n\nsource-repository head\n    type: git\n    location: git://github.com/sw17ch/data-clist.git\n\nLibrary\n    Default-Language: Haskell2010\n    Build-Depends: base >= 4 && < 5,\n                   deepseq >= 1.1 && < 1.5\n    Exposed-Modules:\n        Data.CircularList\n        Data.CircularList.Internal\n\n    ghc-options:        -Wall\n    hs-source-dirs:     src\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type:             exitcode-stdio-1.0\n  Build-Depends:    base >=4.11 && < 5\n                  , data-clist\n                  , QuickCheck >= 2.4 && < 2.15\n  hs-source-dirs:   tests/\n  main-is:          quickcheck.hs\n\n";
    }