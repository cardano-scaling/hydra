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
      specVersion = "2.4";
      identifier = { name = "typerep-map"; version = "0.5.0.0"; };
      license = "MPL-2.0";
      copyright = "2017-2022 Kowainik";
      maintainer = "Kowainik <xrom.xkov@gmail.com>";
      author = "Veronika Romashkina, Vladislav Zavialov, Dmitrii Kovanikov";
      homepage = "https://github.com/kowainik/typerep-map";
      url = "";
      synopsis = "Efficient implementation of a dependent map with types as keys";
      description = "A dependent map from type representations to values of these types.\n\nHere is an illustration of such a map:\n\n>     TMap\n> ---------------\n>  Int  -> 5\n>  Bool -> True\n>  Char -> 'x'\n\nIn addition to @TMap@, we provide @TypeRepMap@ parametrized by a\n@vinyl@-style interpretation. This data structure is equivalent to @DMap\nTypeRep@, but with significantly more efficient lookups.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      sublibs = {
        "typerep-extra-impls" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      tests = {
        "typerep-map-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc-typelits-knownnat" or (errorHandler.buildDepError "ghc-typelits-knownnat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."typerep-map" or (errorHandler.buildDepError "typerep-map"))
            (hsPkgs."typerep-map".components.sublibs.typerep-extra-impls or (errorHandler.buildDepError "typerep-map:typerep-extra-impls"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "typerep-map-benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."ghc-typelits-knownnat" or (errorHandler.buildDepError "ghc-typelits-knownnat"))
            (hsPkgs."typerep-map" or (errorHandler.buildDepError "typerep-map"))
            (hsPkgs."typerep-map".components.sublibs.typerep-extra-impls or (errorHandler.buildDepError "typerep-map:typerep-extra-impls"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/typerep-map-0.5.0.0.tar.gz";
      sha256 = "4a1e806f6d65635a9c068e65d8f4bc2acb314bd95195c6dba351fb48410a502b";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                typerep-map\nversion:             0.5.0.0\nsynopsis:            Efficient implementation of a dependent map with types as keys\ndescription:\n    A dependent map from type representations to values of these types.\n    .\n    Here is an illustration of such a map:\n    .\n    >     TMap\n    > ---------------\n    >  Int  -> 5\n    >  Bool -> True\n    >  Char -> 'x'\n    .\n    In addition to @TMap@, we provide @TypeRepMap@ parametrized by a\n    @vinyl@-style interpretation. This data structure is equivalent to @DMap\n    TypeRep@, but with significantly more efficient lookups.\n\nhomepage:            https://github.com/kowainik/typerep-map\nbug-reports:         https://github.com/kowainik/typerep-map/issues\nlicense:             MPL-2.0\nlicense-file:        LICENSE\nauthor:              Veronika Romashkina, Vladislav Zavialov, Dmitrii Kovanikov\nmaintainer:          Kowainik <xrom.xkov@gmail.com>\ncopyright:           2017-2022 Kowainik\ncategory:            Data, Data Structures, Types\nbuild-type:          Simple\nextra-doc-files:     README.md\n                   , CHANGELOG.md\ntested-with:         GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.1\n\nsource-repository head\n  type:                git\n  location:            https://github.com/kowainik/typerep-map.git\n\ncommon common-options\n  build-depends:       base >= 4.10 && < 4.17\n\n  default-language:    Haskell2010\n  default-extensions:  BangPatterns\n                       DerivingStrategies\n                       OverloadedStrings\n                       RecordWildCards\n                       ScopedTypeVariables\n                       TypeApplications\n  ghc-options:         -Wall\n                       -Wcompat\n                       -Widentities\n                       -Wincomplete-uni-patterns\n                       -Wincomplete-record-updates\n                       -Wredundant-constraints\n                       -fhide-source-paths\n  if impl(ghc >= 8.4)\n    ghc-options:       -Wmissing-export-lists\n                       -Wpartial-fields\n  if impl(ghc >= 8.8)\n    ghc-options:       -Wmissing-deriving-strategies\n                       -Werror=missing-deriving-strategies\n  if impl(ghc >= 8.10)\n    ghc-options:       -Wunused-packages\n\nlibrary\n  import:              common-options\n  hs-source-dirs:      src\n  exposed-modules:     Data.TMap\n                       Data.TypeRepMap\n                       Data.TypeRepMap.Internal\n\n  build-depends:       ghc-prim >= 0.5.1.1 && < 0.9\n                     , primitive ^>= 0.7.0\n                     , deepseq ^>= 1.4\n\nlibrary typerep-extra-impls\n  import:              common-options\n  hs-source-dirs:      typerep-extra-impls\n  exposed-modules:     Data.TypeRep.CMap\n                       Data.TypeRep.OptimalVector\n                       Data.TypeRep.Vector\n\n  build-depends:       containers >= 0.5.10.2 && < 0.7\n                     , vector ^>= 0.12.0.1\n                     , deepseq ^>= 1.4\n\ntest-suite typerep-map-test\n  import:              common-options\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n\n  main-is:             Test.hs\n  other-modules:       Test.TypeRep.CMap\n                     , Test.TypeRep.TypeRepMap\n                     , Test.TypeRep.TypeRepMapProperty\n                     , Test.TypeRep.Vector\n                     , Test.TypeRep.VectorOpt\n\n  build-depends:       ghc-typelits-knownnat >= 0.4.2 && < 0.8\n                     , hedgehog >= 1.0 && < 1.2\n                     , hspec >= 2.7.1 && < 2.10\n                     , hspec-hedgehog ^>= 0.0.1\n                     , typerep-map\n                     , typerep-extra-impls\n\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n\nbenchmark typerep-map-benchmark\n  import:              common-options\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      benchmark\n\n  main-is:             Main.hs\n  other-modules:       CMap\n                     , CacheMap\n                     , DMap\n                     , Spec\n                     , Vector\n                     , OptimalVector\n\n  build-depends:       criterion >= 1.4.1.0 && < 1.6\n                     , deepseq ^>= 1.4.3.0\n                     , dependent-map >= 0.2.4.0 && < 0.5\n                     , dependent-sum >= 0.5 && < 0.8\n                     , ghc-typelits-knownnat >= 0.4.2 && < 0.8\n                     , typerep-map\n                     , typerep-extra-impls\n\n  ghc-options:         -O2 -threaded -rtsopts -with-rtsopts=-N -freduction-depth=0\n";
    }