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
      identifier = { name = "insert-ordered-containers"; version = "0.2.5.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/insert-ordered-containers#readme";
      url = "";
      synopsis = "Associative containers retaining insertion order for traversals.";
      description = "Associative containers retaining insertion order for traversals.\n\nThe implementation is based on `unordered-containers`.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."optics-extra" or (errorHandler.buildDepError "optics-extra"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "ins-ord-containers-tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."insert-ordered-containers" or (errorHandler.buildDepError "insert-ordered-containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/insert-ordered-containers-0.2.5.1.tar.gz";
      sha256 = "f9a8217d7afa8ddeb7e6fbe1e4c37e40e31d26561acf9d8b01e8f7e3d703ccd6";
      });
    }) // {
    package-description-override = "name:               insert-ordered-containers\nversion:            0.2.5.1\nx-revision:         3\nsynopsis:\n  Associative containers retaining insertion order for traversals.\n\ndescription:\n  Associative containers retaining insertion order for traversals.\n  .\n  The implementation is based on `unordered-containers`.\n\ncategory:           Web\nhomepage:           https://github.com/phadej/insert-ordered-containers#readme\nbug-reports:        https://github.com/phadej/insert-ordered-containers/issues\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\nbuild-type:         Simple\ncabal-version:      >=1.10\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.2\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/insert-ordered-containers\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  build-depends:\n      aeson                 >=1.4.2.0  && <1.6 || >=2.0.0.0 && <2.2\n    , base                  >=4.9      && <4.18\n    , deepseq               >=1.4.2.0  && <1.5\n    , hashable              >=1.2.6.1  && <1.5\n    , indexed-traversable   >=0.1.1    && <0.2\n    , lens                  >=4.17     && <5.3\n    , optics-core           >=0.2      && <0.5\n    , optics-extra          >=0.2      && <0.5\n    , semigroupoids         >=5.3.2    && <5.4\n    , text                  >=1.2.3.0  && <1.3 || >=2.0 && <2.1\n    , transformers          >=0.5.2.0  && <0.6\n    , unordered-containers  >=0.2.14.0 && <0.3\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups    >=0.18.5  && <0.21\n      , transformers  >=0.5.2.0 && <0.6\n\n  exposed-modules:\n    Data.HashMap.Strict.InsOrd\n    Data.HashSet.InsOrd\n\n  other-modules:    Data.HashMap.InsOrd.Internal\n\ntest-suite ins-ord-containers-tests\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   test\n  ghc-options:      -Wall\n\n  -- inherited from library\n  build-depends:\n      aeson\n    , base\n    , base-compat\n    , hashable\n    , insert-ordered-containers\n    , lens\n    , QuickCheck                 >=2.13.2   && <2.15\n    , semigroupoids\n    , tasty                      >=0.10.1.2 && <1.5\n    , tasty-quickcheck           >=0.8.3.2  && <0.11\n    , text\n    , unordered-containers\n";
    }