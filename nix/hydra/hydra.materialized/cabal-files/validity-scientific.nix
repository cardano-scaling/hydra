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
      specVersion = "1.12";
      identifier = { name = "validity-scientific"; version = "0.2.0.3"; };
      license = "MIT";
      copyright = "Copyright: (c) 2017-2020 Tom Sydney Kerckhove";
      maintainer = "syd@cs-syd.eu";
      author = "Tom Sydney Kerckhove";
      homepage = "https://github.com/NorfairKing/validity#readme";
      url = "";
      synopsis = "Validity instances for scientific";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."validity" or (errorHandler.buildDepError "validity"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/validity-scientific-0.2.0.3.tar.gz";
      sha256 = "773a4a35933637d0bade859dd0e8acadc781d9ccd3b057d60e7ffaaa20e5186f";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.33.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 62648309d590d2b89fde07fbce02c54ba5ec84c0400fdb8705b0254e5e703af9\n\nname:           validity-scientific\nversion:        0.2.0.3\nsynopsis:       Validity instances for scientific\ncategory:       Web\nhomepage:       https://github.com/NorfairKing/validity#readme\nbug-reports:    https://github.com/NorfairKing/validity/issues\nauthor:         Tom Sydney Kerckhove\nmaintainer:     syd@cs-syd.eu\ncopyright:      Copyright: (c) 2017-2020 Tom Sydney Kerckhove\nlicense:        MIT\nlicense-file:   LICENSE\nbuild-type:     Simple\n\nsource-repository head\n  type: git\n  location: https://github.com/NorfairKing/validity\n\nlibrary\n  exposed-modules:\n      Data.Validity.Scientific\n  other-modules:\n      Paths_validity_scientific\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.7 && <5\n    , scientific\n    , validity >=0.5\n  default-language: Haskell2010\n";
    }