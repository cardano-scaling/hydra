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
      identifier = { name = "tracer-transformers"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "tracer transformers and examples showing their use";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        };
      exes = {
        "tracer-transfomers-example1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          };
        "tracer-transfomers-example2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/tracer-transformers-0.1.0.1.tar.gz";
      sha256 = "f7d532f74edd52a8529a2389a91685b172fcde32f8e9b27f1dbd90597799fc9d";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                tracer-transformers\nversion:             0.1.0.1\nsynopsis:            tracer transformers and examples showing their use\n-- description:\n-- bug-reports:\nlicense:             Apache-2.0\nlicense-files:       LICENSE, NOTICE\nauthor:              Neil Davies, Alexander Diemand, Andreas Triantafyllos\nmaintainer:          operations@iohk.io\ncopyright:           2019 IOHK\ncategory:            Logging\nbuild-type:          Simple\n-- extra-source-files:\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Control.Tracer.Transformers\n                       Control.Tracer.Transformers.ObserveOutcome\n                       Control.Tracer.Transformers.WithThreadAndTime\n\n  default-extensions:  OverloadedStrings\n  default-language:    Haskell2010\n  build-depends:       base\n                     , contra-tracer\n                     , time\n                     , safe-exceptions\n  if impl(ghc < 8.5)\n    build-depends:     contravariant\n  ghc-options:         -Wall -Werror\n\nexecutable tracer-transfomers-example1\n  main-is:             Main.hs\n  hs-source-dirs:      example1\n  -- other-modules:\n  -- other-extensions:\n  build-depends:       base\n                     , contra-tracer\n                     , time\n                     , tracer-transformers\n  default-extensions:  OverloadedStrings\n  default-language:    Haskell2010\n\nexecutable tracer-transfomers-example2\n  main-is:             Main.hs\n  hs-source-dirs:      example2\n  -- other-modules:\n  -- other-extensions:\n  build-depends:       base\n                     , contra-tracer\n                     , text\n                     , tracer-transformers\n  default-extensions:  OverloadedStrings\n  default-language:    Haskell2010\n";
    }