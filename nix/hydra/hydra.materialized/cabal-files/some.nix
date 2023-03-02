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
    flags = { newtype-unsafe = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "some"; version = "1.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "James Cook <mokus@deepbondi.net>, Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/some";
      url = "";
      synopsis = "Existential type: Some";
      description = "This library defines an existential type 'Some'.\n\n@\ndata Some f where\n\\    Some :: f a -> Some f\n@\n\nin few variants, and utilities to work with it.\n\nIf you are unsure which variant to use, use the one in \"Data.Some\" module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ];
        buildable = true;
        };
      tests = {
        "hkd-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."some" or (errorHandler.buildDepError "some"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/some-1.0.2.tar.gz";
      sha256 = "ccf8a4b07d5236a6f966649ebef39e764f1f6bb52217647e2e96d0cdfe2bbb8a";
      });
    }) // {
    package-description-override = "name:               some\nversion:            1.0.2\nstability:          provisional\ncabal-version:      >=1.10\nbuild-type:         Simple\nauthor:\n  James Cook <mokus@deepbondi.net>, Oleg Grenrus <oleg.grenrus@iki.fi>\n\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\nhomepage:           https://github.com/phadej/some\ncategory:           Data, Dependent Types\nsynopsis:           Existential type: Some\ndescription:\n  This library defines an existential type 'Some'.\n  .\n  @\n  data Some f where\n  \\    Some :: f a -> Some f\n  @\n  .\n  in few variants, and utilities to work with it.\n  .\n  If you are unsure which variant to use, use the one in \"Data.Some\" module.\n\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.3\n\nextra-source-files: ChangeLog.md\n\nflag newtype-unsafe\n  description:\n    Use implementation using @newtype@ and unsafe @Any@, instead of GADT\n\n  manual:      True\n  default:     True\n\nsource-repository head\n  type:     git\n  location: git://github.com/phadej/some.git\n  subdir:   some\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n\n  if flag(newtype-unsafe)\n    cpp-options: -DSOME_NEWTYPE\n\n  exposed-modules:\n    Data.GADT.Compare\n    Data.GADT.DeepSeq\n    Data.GADT.Show\n    Data.Some\n    Data.Some.Church\n    Data.Some.GADT\n    Data.Some.Newtype\n\n  other-modules:    Data.GADT.Internal\n  build-depends:\n      base     >=4.3     && <4.16\n    , deepseq  >=1.3.0.0 && <1.5\n\n  if !impl(ghc >=7.8)\n    build-depends: type-equality >=1 && <1.1\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups           >=0.18.5 && <0.20\n      , transformers         >=0.3    && <0.6\n      , transformers-compat  >=0.6    && <0.7\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite hkd-example\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          HKD.hs\n  build-depends:\n      base\n    , some\n";
    }