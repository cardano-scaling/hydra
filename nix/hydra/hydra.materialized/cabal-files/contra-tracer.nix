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
      identifier = { name = "contra-tracer"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "A simple interface for logging, tracing or monitoring.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/contra-tracer-0.1.0.0.tar.gz";
      sha256 = "f6d8d40f65d3bfc5126a9fa84d56184cddc06c6869da9c5e6602c4719ca9088d";
      });
    }) // {
    package-description-override = "name:                contra-tracer\nversion:             0.1.0.0\nsynopsis:            A simple interface for logging, tracing or monitoring.\n-- description:\nlicense:             Apache-2.0\nlicense-files:       LICENSE, NOTICE\nauthor:              Neil Davies, Alexander Diemand, Andreas Triantafyllos\nmaintainer:          operations@iohk.io\ncopyright:           2019 IOHK\ncategory:            Logging\nbuild-type:          Simple\nextra-source-files:  README.md\ncabal-version:       >=1.10\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Control.Tracer\n                       Control.Tracer.Observe\n\n  default-language:    Haskell2010\n  build-depends:       base\n  if impl(ghc < 8.5)\n    build-depends:     contravariant\n  ghc-options:         -Wall -Werror\n";
    }