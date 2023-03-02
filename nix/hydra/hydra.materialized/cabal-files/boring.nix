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
    flags = { tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "boring"; version = "0.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/boring";
      url = "";
      synopsis = "Boring and Absurd types";
      description = "* @Boring@ types are isomorphic to @()@.\n\n* @Absurd@ types are isomorphic to @Void@.\n\nSee [What does () mean in Haskell -answer by Conor McBride](https://stackoverflow.com/questions/33112439/what-does-mean-in-haskell/33115522#33115522)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (flags.tagged || !(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/boring-0.2.tar.gz";
      sha256 = "0bac533b66e754d4fc65ab8d7557eea6f7b35d16998e5e74579b25a372aa4c34";
      });
    }) // {
    package-description-override = "name:               boring\nversion:            0.2\nx-revision:         2\nsynopsis:           Boring and Absurd types\ndescription:\n  * @Boring@ types are isomorphic to @()@.\n  . \n  * @Absurd@ types are isomorphic to @Void@.\n  .\n  See [What does () mean in Haskell -answer by Conor McBride](https://stackoverflow.com/questions/33112439/what-does-mean-in-haskell/33115522#33115522)\n\nhomepage:           https://github.com/phadej/boring\nbug-reports:        https://github.com/phadej/boring/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2017-2021 Oleg Grenrus\ncategory:           Data\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ncabal-version:      >=1.10\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/boring.git\n  subdir:   boring\n\nflag tagged\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\n  default:     True\n  manual:      True\n\nlibrary\n  exposed-modules:  Data.Boring\n  build-depends:\n      base          >=4.5 && <4.18\n    , transformers  >=0.3 && <0.7\n\n  if impl(ghc <7.6)\n    build-depends: ghc-prim\n  \n  if !impl(ghc >=7.8)\n    build-depends: type-equality >=1 && <1.1\n\n  if !impl(ghc >=7.10)\n    build-depends: void >=0.7.3 && <0.8\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups           >=0.18.5 && <0.21\n      , transformers-compat  >=0.5    && <0.8\n\n  if flag(tagged) || !impl(ghc >=7.8)\n    build-depends: tagged >=0.8.6 && <0.9\n\n  other-extensions:\n    CPP\n    DefaultSignatures\n    FlexibleContexts\n    GADTs\n    Trustworthy\n    TypeOperators\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n";
    }