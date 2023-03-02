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
      identifier = { name = "dictionary-sharing"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "jonas.duregard@chalmers.se";
      author = "Jonas Duregård";
      homepage = "";
      url = "";
      synopsis = "Sharing/memoization of class members";
      description = "Library for ensuring that class members are shared.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dictionary-sharing-0.1.0.0.tar.gz";
      sha256 = "8c3b5184d5d6056433d51a49c5402e4ab7b0260073d5342685b8e141d2be5a01";
      });
    }) // {
    package-description-override = "name:                dictionary-sharing\r\nversion:             0.1.0.0\r\nx-revision: 3\r\nsynopsis:            Sharing/memoization of class members\r\ndescription:         Library for ensuring that class members are shared.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Jonas Duregård\r\nmaintainer:          jonas.duregard@chalmers.se\r\ncategory:            Development\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\n\r\nsource-repository head\r\n  type:      git\r\n  location:  https://github.com/JonasDuregard/dictionary-sharing\r\n\r\nlibrary\r\n  exposed-modules:     Data.ClassSharing\r\n  -- other-modules:\r\n  -- other-extensions:\r\n  build-depends:       base >=4.7 && <5, containers >=0.5 && <0.7\r\n  -- hs-source-dirs:\r\n  default-language:    Haskell2010\r\n";
    }