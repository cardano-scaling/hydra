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
      specVersion = "1.18";
      identifier = { name = "vty"; version = "5.37"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jonathan Daugherty (cygnus@foobox.com)";
      author = "AUTHORS";
      homepage = "https://github.com/jtdaugherty/vty";
      url = "";
      synopsis = "A simple terminal UI library";
      description = "vty is terminal GUI library in the niche of ncurses. It is intended to\nbe easy to use, have no confusing corner cases, and good support for\ncommon terminal types.\n\nSee the @vty-examples@ package as well as the program\n@test/interactive_terminal_test.hs@ included in the @vty@ package for\nexamples on how to use the library.\n\nImport the \"Graphics.Vty\" convenience module to get access to the core\nparts of the library.\n\n&#169; 2006-2007 Stefan O'Rear; BSD3 license.\n\n&#169; Corey O'Connor; BSD3 license.\n\n&#169; Jonathan Daugherty; BSD3 license.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        };
      exes = {
        "vty-build-width-table" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "vty-mode-demo" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        "vty-demo" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      tests = {
        "verify-using-mock-terminal" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-terminal" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-display-attributes" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-empty-image-props" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-eval-terminfo-caps" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-image-ops" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-image-trans" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-inline" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-parse-terminfo-caps" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-simple-span-generation" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-crop-span-generation" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-layers-span-generation" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-color-mapping" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-utf8-width" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-using-mock-input" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."quickcheck-assertions" or (errorHandler.buildDepError "quickcheck-assertions"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-smallcheck" or (errorHandler.buildDepError "test-framework-smallcheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "verify-config" = {
          depends = [
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."quickcheck-assertions" or (errorHandler.buildDepError "quickcheck-assertions"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-smallcheck" or (errorHandler.buildDepError "test-framework-smallcheck"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."string-qq" or (errorHandler.buildDepError "string-qq"))
            (hsPkgs."terminfo" or (errorHandler.buildDepError "terminfo"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vty-5.37.tar.gz";
      sha256 = "67e1376b735232c3a5e7fa3c3eff00fdc457bdd5ba7dcf3a758aee6b8b60cdf0";
      });
    }) // {
    package-description-override = "name:                vty\nversion:             5.37\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              AUTHORS\nmaintainer:          Jonathan Daugherty (cygnus@foobox.com)\nhomepage:            https://github.com/jtdaugherty/vty\ncategory:            User Interfaces\nsynopsis:            A simple terminal UI library\ndescription:\n  vty is terminal GUI library in the niche of ncurses. It is intended to\n  be easy to use, have no confusing corner cases, and good support for\n  common terminal types.\n  .\n  See the @vty-examples@ package as well as the program\n  @test/interactive_terminal_test.hs@ included in the @vty@ package for\n  examples on how to use the library.\n  .\n  Import the \"Graphics.Vty\" convenience module to get access to the core\n  parts of the library.\n  .\n  &#169; 2006-2007 Stefan O'Rear; BSD3 license.\n  .\n  &#169; Corey O'Connor; BSD3 license.\n  .\n  &#169; Jonathan Daugherty; BSD3 license.\ncabal-version:       1.18\nbuild-type:          Simple\nextra-doc-files:     README.md,\n                     AUTHORS,\n                     CHANGELOG.md,\n                     LICENSE\ntested-with:         GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.3, GHC==8.6.5\n\nsource-repository head\n  type: git\n  location: https://github.com/jtdaugherty/vty.git\n\nlibrary\n  default-language:    Haskell2010\n  build-depends:       base >= 4.8 && < 5,\n                       blaze-builder >= 0.3.3.2 && < 0.5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       directory,\n                       filepath >= 1.0 && < 2.0,\n                       microlens < 0.4.14,\n                       microlens-mtl,\n                       microlens-th,\n                       hashable >= 1.2,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       parallel >= 2.2 && < 3.3,\n                       parsec >= 2 && < 4,\n                       stm,\n                       terminfo >= 0.3 && < 0.5,\n                       transformers >= 0.3.0.0,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7,\n                       binary,\n                       ansi-terminal >= 0.10.3\n\n  if !impl(ghc >= 8.0)\n    build-depends:     semigroups >= 0.16,\n                       fail\n\n  exposed-modules:     Graphics.Vty\n                       Graphics.Vty.Attributes\n                       Graphics.Vty.Attributes.Color \n                       Graphics.Vty.Attributes.Color240\n                       Graphics.Vty.Config\n                       Graphics.Vty.Error\n                       Graphics.Vty.Image\n                       Graphics.Vty.Inline\n                       Graphics.Vty.Inline.Unsafe\n                       Graphics.Vty.Input\n                       Graphics.Vty.Input.Events\n                       Graphics.Vty.Picture\n                       Graphics.Vty.Output\n                       Graphics.Text.Width\n                       Codec.Binary.UTF8.Debug\n                       Data.Terminfo.Parse\n                       Data.Terminfo.Eval\n                       Graphics.Vty.Debug\n                       Graphics.Vty.DisplayAttributes\n                       Graphics.Vty.Image.Internal\n                       Graphics.Vty.Input.Classify\n                       Graphics.Vty.Input.Classify.Types\n                       Graphics.Vty.Input.Classify.Parse\n                       Graphics.Vty.Input.Loop\n                       Graphics.Vty.Input.Mouse\n                       Graphics.Vty.Input.Focus\n                       Graphics.Vty.Input.Paste\n                       Graphics.Vty.Input.Terminfo\n                       Graphics.Vty.PictureToSpans\n                       Graphics.Vty.Span\n                       Graphics.Vty.Output.Mock\n                       Graphics.Vty.Output.Interface\n                       Graphics.Vty.Output.XTermColor\n                       Graphics.Vty.Output.TerminfoBased\n                       Graphics.Vty.UnicodeWidthTable.Types\n                       Graphics.Vty.UnicodeWidthTable.IO\n                       Graphics.Vty.UnicodeWidthTable.Query\n                       Graphics.Vty.UnicodeWidthTable.Install\n\n  other-modules:       Graphics.Vty.Debug.Image\n                       Graphics.Vty.Input.Terminfo.ANSIVT\n\n  c-sources:           cbits/gwinsz.c\n                       cbits/set_term_timing.c\n                       cbits/get_tty_erase.c\n                       cbits/mk_wcwidth.c\n\n  include-dirs:        cbits\n\n  hs-source-dirs:      src\n\n  ghc-options:         -O2 -funbox-strict-fields -Wall -fspec-constr -fspec-constr-count=10\n\n  ghc-prof-options:    -O2 -funbox-strict-fields -caf-all -Wall -fspec-constr -fspec-constr-count=10\n\nexecutable vty-build-width-table\n  main-is:             BuildWidthTable.hs\n  hs-source-dirs:      tools\n\n  default-language:    Haskell2010\n  ghc-options:         -threaded -Wall\n\n  if !impl(ghc >= 8.0)\n    build-depends:     semigroups >= 0.16\n\n  build-depends:       vty,\n                       directory,\n                       filepath,\n                       base >= 4.8 && < 5\n\nexecutable vty-mode-demo\n  main-is:             ModeDemo.hs\n  hs-source-dirs:      demos\n\n  default-language:    Haskell2010\n  ghc-options:         -threaded\n\n  build-depends:       vty,\n                       base >= 4.8 && < 5,\n                       containers,\n                       microlens,\n                       microlens-mtl,\n                       mtl >= 1.1.1.0 && < 2.3\n\nexecutable vty-demo\n  main-is:             Demo.hs\n  hs-source-dirs:      demos\n\n  default-language:    Haskell2010\n  ghc-options:         -threaded\n\n  build-depends:       vty,\n                       base >= 4.8 && < 5,\n                       containers,\n                       microlens,\n                       microlens-mtl,\n                       mtl >= 1.1.1.0 && < 2.3\n\ntest-suite verify-using-mock-terminal\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyUsingMockTerminal\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n                       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       terminfo >= 0.3 && < 0.5,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-terminal\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyOutput\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n                       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       terminfo >= 0.3 && < 0.5,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-display-attributes\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyDisplayAttributes\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.DisplayAttributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-empty-image-props\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyEmptyImageProps\n\n  other-modules:       Verify\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-eval-terminfo-caps\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyEvalTerminfoCaps\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       blaze-builder >= 0.3.3.2 && < 0.5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       terminfo >= 0.3 && < 0.5,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-image-ops\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyImageOps\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Image\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-image-trans\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyImageTrans\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Image\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-inline\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyInline\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-parse-terminfo-caps\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyParseTerminfoCaps\n\n  other-modules:       Verify\n                       Verify.Data.Terminfo.Parse\n                       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       terminfo >= 0.3 && < 0.5,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-simple-span-generation\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifySimpleSpanGeneration\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\n\ntest-suite verify-crop-span-generation\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyCropSpanGeneration\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\n\ntest-suite verify-layers-span-generation\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyLayersSpanGeneration\n\n  other-modules:       Verify\n                       Verify.Graphics.Vty.Attributes\n                       Verify.Graphics.Vty.Prelude\n                       Verify.Graphics.Vty.Picture\n                       Verify.Graphics.Vty.Image\n                       Verify.Graphics.Vty.Span\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-color-mapping\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyColor240\n\n  other-modules:       Verify\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-utf8-width\n  default-language:    Haskell2010\n\n  type:                detailed-0.9\n\n  hs-source-dirs:      test\n\n  test-module:         VerifyUtf8Width\n\n  other-modules:       Verify\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\ntest-suite verify-using-mock-input\n  default-language:    Haskell2010\n\n  type:                exitcode-stdio-1.0\n\n  hs-source-dirs:      test\n\n  main-is:             VerifyUsingMockInput.hs\n\n  other-modules:       Verify.Graphics.Vty.Output\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       QuickCheck >= 2.7,\n                       smallcheck == 1.*,\n                       quickcheck-assertions >= 0.1.1,\n                       test-framework == 0.8.*,\n                       test-framework-smallcheck == 0.2.*,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       microlens,\n                       microlens-mtl,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       stm,\n                       terminfo >= 0.3 && < 0.5,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\n  ghc-options:         -threaded -Wall\n\ntest-suite verify-config\n  default-language:    Haskell2010\n\n  type:                exitcode-stdio-1.0\n\n  hs-source-dirs:      test\n\n  main-is:             VerifyConfig.hs\n\n  build-depends:       vty,\n                       Cabal >= 1.20,\n                       HUnit,\n                       QuickCheck >= 2.7,\n                       smallcheck == 1.*,\n                       quickcheck-assertions >= 0.1.1,\n                       test-framework == 0.8.*,\n                       test-framework-smallcheck == 0.2.*,\n                       test-framework-hunit,\n                       random >= 1.0 && < 1.3,\n                       base >= 4.8 && < 5,\n                       bytestring,\n                       containers,\n                       deepseq >= 1.1 && < 1.5,\n                       microlens,\n                       microlens-mtl,\n                       mtl >= 1.1.1.0 && < 2.3,\n                       string-qq,\n                       terminfo >= 0.3 && < 0.5,\n                       text >= 0.11.3,\n                       unix,\n                       utf8-string >= 0.3 && < 1.1,\n                       vector >= 0.7\n\n  ghc-options:         -threaded -Wall\n";
    }