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
      identifier = { name = "dec"; version = "0.0.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2019-2021 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/vec";
      url = "";
      synopsis = "Decidable propositions.";
      description = "This package provides a @Dec@ type.\n\n@\ntype Neg a = a -> Void\n\ndata Dec a\n\\    = Yes a\n\\    | No (Neg a)\n@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."boring" or (errorHandler.buildDepError "boring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dec-0.0.5.tar.gz";
      sha256 = "e18043c7c7e68168489f164921a02efede0c0175a5012ed5451ec2242338df88";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               dec\nversion:            0.0.5\nsynopsis:           Decidable propositions.\ncategory:           Data, Dependent Types\ndescription:\n  This package provides a @Dec@ type.\n  .\n  @\n  type Neg a = a -> Void\n  .\n  data Dec a\n  \\    = Yes a\n  \\    | No (Neg a)\n  @\n\nhomepage:           https://github.com/phadej/vec\nbug-reports:        https://github.com/phadej/vec/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2019-2021 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/dec.git\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall -fprint-explicit-kinds\n  exposed-modules:  Data.Type.Dec\n  build-depends:\n      base    >=4.7 && <4.18\n    , boring  >=0.2 && <0.3\n\n  if !impl(ghc >=7.10)\n    build-depends: void >=0.7.3 && <0.8\n\n  if impl(ghc >=9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n";
    }