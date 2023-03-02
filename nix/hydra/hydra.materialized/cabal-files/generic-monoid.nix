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
      identifier = { name = "generic-monoid"; version = "0.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "2018 Luke Clifton";
      maintainer = "lukec@themk.net";
      author = "Luke Clifton";
      homepage = "";
      url = "";
      synopsis = "Derive monoid instances for product types.";
      description = "Using GHC's generics, allow for deriving `Monoid` and `Semigroup` instances for your product types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generic-monoid-0.1.0.1.tar.gz";
      sha256 = "cfd072ad70af41c1b94ac24e42e2635f37ed2a54e8f4be871be78b18b66b2adf";
      });
    }) // {
    package-description-override = "name:                generic-monoid\r\nversion:             0.1.0.1\r\nx-revision: 1\r\nsynopsis:            Derive monoid instances for product types.\r\ndescription:         Using GHC's generics, allow for deriving `Monoid` and `Semigroup` instances for your product types.\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Luke Clifton\r\nmaintainer:          lukec@themk.net\r\ncopyright:           2018 Luke Clifton\r\ncategory:            Data\r\nbuild-type:          Simple\r\nextra-source-files:  ChangeLog.md\r\ncabal-version:       >=1.10\r\n\r\nlibrary\r\n  exposed-modules:     Data.Semigroup.Generic, Data.Monoid.Generic\r\n  build-depends:       base >=4.11 && <4.17\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n";
    }