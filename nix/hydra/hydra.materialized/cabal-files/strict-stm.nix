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
    flags = { checktvarinvariant = false; asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "strict-stm"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Strict STM interface polymorphic over stm implementation.";
      description = "The `strict-stm` package gives a strict interface to stm,\ncurrently either one provided by `stm` package for the\n`IO` monad or `io-sim` package for the `IOSim` monad.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/strict-stm-0.1.0.0.tar.gz";
      sha256 = "371750cb398045857bd7adafe6995af60c0b854f39ee2b0252ce39338dd5616b";
      });
    }) // {
    package-description-override = "name:                strict-stm\nversion:             0.1.0.0\nsynopsis:            Strict STM interface polymorphic over stm implementation.\ndescription:         The `strict-stm` package gives a strict interface to stm,\n                     currently either one provided by `stm` package for the\n                     `IO` monad or `io-sim` package for the `IOSim` monad.\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\ncopyright:           2019-2021 Input Output (Hong Kong) Ltd.\nauthor:              Alexander Vieth, Marcin Szamotulski, Duncan Coutts\nmaintainer:\ncategory:            Control\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/ouroboros-network\n  subdir:   strict-stm\n\nflag checktvarinvariant\n  Description: Enable runtime invariant checks on StrictT(M)Var\n  Manual: True\n  Default: False\n\nflag asserts\n  description: Enable assertions\n  manual:      False\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n\n  exposed-modules:     Control.Monad.Class.MonadSTM.Strict\n  default-language:    Haskell2010\n  build-depends:       base  >=4.9 && <4.15,\n                       stm   >=2.5 && <2.6,\n                       io-classes\n  ghc-options:         -Wall\n                       -Wno-unticked-promoted-constructors\n                       -Wcompat\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wpartial-fields\n                       -Widentities\n\n  if flag(asserts)\n    ghc-options: -fno-ignore-asserts\n\n  if flag(checktvarinvariant)\n    cpp-options: -DCHECK_TVAR_INVARIANT\n";
    }