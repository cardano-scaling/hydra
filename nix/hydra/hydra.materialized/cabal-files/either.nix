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
      identifier = { name = "either"; version = "5.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2017 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/either/";
      url = "";
      synopsis = "Combinators for working with sums";
      description = "Combinators for working with sums.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/either-5.0.2.tar.gz";
      sha256 = "bfa45b967fd77b1c7c952e156377441e3c42b6dc873ab1f06a7421a3222287be";
      });
    }) // {
    package-description-override = "name:          either\ncategory:      Control, Monads\nversion:       5.0.2\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/either/\nbug-reports:   http://github.com/ekmett/either/issues\ncopyright:     Copyright (C) 2008-2017 Edward A. Kmett\nsynopsis:      Combinators for working with sums\ndescription:   Combinators for working with sums.\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.4\n             , GHC == 9.0.1\nextra-source-files:\n  .gitignore\n  .ghci\n  .vim.custom\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/either.git\n\nlibrary\n  build-depends:\n    base              >= 4       && < 5,\n    bifunctors        >= 4       && < 6,\n    mtl               >= 2.0     && < 2.4,\n    profunctors       >= 4       && < 6,\n    semigroupoids     >= 4       && < 6\n\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.8.3.1 && < 1\n\n  other-extensions: CPP Rank2Types\n  ghc-options: -Wall\n  hs-source-dirs: src\n  default-language: Haskell2010\n  exposed-modules:\n    Data.Either.Combinators\n    Data.Either.Validation\n\ntest-suite tests\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: tests\n  build-depends:\n    base,\n    either,\n    test-framework             >= 0.8.1.1 && < 0.9,\n    test-framework-quickcheck2 >= 0.3.0.3 && < 0.4,\n    QuickCheck                 >= 2.9     && < 2.15\n  default-language: Haskell2010\n";
    }