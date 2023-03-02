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
      identifier = { name = "process-extras"; version = "0.7.4"; };
      license = "MIT";
      copyright = "";
      maintainer = "David Fox <dsf@seereason.com>";
      author = "David Lazar, Bas van Dijk, David Fox";
      homepage = "https://github.com/seereason/process-extras";
      url = "";
      synopsis = "Process extras";
      description = "Extends <http://hackage.haskell.org/package/process>.\nRead process input and output as ByteStrings or\nText, or write your own ProcessOutput instance.\nLazy process input and output.  ProcessMaker class\nfor more flexibility in the process creation API.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."ListLike" or (errorHandler.buildDepError "ListLike"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
          ];
        buildable = true;
        };
      tests = {
        "process-extras-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/process-extras-0.7.4.tar.gz";
      sha256 = "293e75f849254ce0ce0d7fa659681917e07a557c602505a2f9e20777467e984e";
      });
    }) // {
    package-description-override = "Name:               process-extras\nVersion:            0.7.4\nSynopsis:           Process extras\nDescription:        Extends <http://hackage.haskell.org/package/process>.\n                    Read process input and output as ByteStrings or\n                    Text, or write your own ProcessOutput instance.\n                    Lazy process input and output.  ProcessMaker class\n                    for more flexibility in the process creation API.\nHomepage:           https://github.com/seereason/process-extras\nLicense:            MIT\nLicense-file:       LICENSE\nAuthor:             David Lazar, Bas van Dijk, David Fox\nMaintainer:         David Fox <dsf@seereason.com>\nCategory:           System\nBuild-type:         Simple\nCabal-version:      >=1.10\nExtra-source-files:\n  README.md, .travis.yml, .ghci, changelog\nTested-With:        GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC >= 8\n\nsource-repository head\n  Type:             git\n  Location:         https://github.com/seereason/process-extras\n\nLibrary\n  Default-Language: Haskell2010\n  ghc-options:      -Wall\n\n  Hs-source-dirs:   src\n\n  Exposed-modules:\n    System.Process.ByteString\n    System.Process.ByteString.Lazy\n    System.Process.Chars\n    System.Process.Common\n    System.Process.ListLike\n    System.Process.Run\n    System.Process.Text\n    System.Process.Text.Lazy\n    System.Process.Text.Builder\n\n  Other-modules:\n    Utils\n\n  Build-depends:\n    base >= 4.5 && < 5,\n    data-default,\n    ListLike >= 4.4,\n    mtl,\n    process,\n    bytestring,\n    text,\n    deepseq >= 1.1,\n    generic-deriving >= 1.10\n\nTest-Suite process-extras-tests\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n  Hs-Source-Dirs: .\n  Main-Is: Tests.hs\n  Build-Depends: base, HUnit, process-extras\n";
    }