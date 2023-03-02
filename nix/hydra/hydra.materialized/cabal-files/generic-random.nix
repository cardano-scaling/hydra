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
    flags = { enable-inspect = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "generic-random"; version = "1.5.0.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "lysxia@gmail.com";
      author = "Li-yao Xia";
      homepage = "http://github.com/lysxia/generic-random";
      url = "";
      synopsis = "Generic random generators for QuickCheck";
      description = "Derive instances of @Arbitrary@ for QuickCheck,\nwith various options to customize implementations.\n\nFor more information\n\n- See the README\n\n- \"Generic.Random.Tutorial\"\n\n- http://blog.poisson.chat/posts/2018-01-05-generic-random-tour.html";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            ];
          buildable = true;
          };
        "coherence" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            ];
          buildable = true;
          };
        "inspect" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            ] ++ (pkgs.lib).optional (!(!flags.enable-inspect)) (hsPkgs."random" or (errorHandler.buildDepError "random"));
          buildable = if !flags.enable-inspect then false else true;
          };
        "inspect-derivingvia" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
            ] ++ (pkgs.lib).optional (!(!flags.enable-inspect)) (hsPkgs."random" or (errorHandler.buildDepError "random"));
          buildable = if !flags.enable-inspect then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generic-random-1.5.0.1.tar.gz";
      sha256 = "dd3451808788d99211edeac27287db5417e97234ce9221a2eb9ab02e9cfc2c0a";
      });
    }) // {
    package-description-override = "name:                generic-random\nversion:             1.5.0.1\nsynopsis:            Generic random generators for QuickCheck\ndescription:\n    Derive instances of @Arbitrary@ for QuickCheck,\n    with various options to customize implementations.\n    .\n    For more information\n    .\n    - See the README\n    .\n    - \"Generic.Random.Tutorial\"\n    .\n    - http://blog.poisson.chat/posts/2018-01-05-generic-random-tour.html\n\nhomepage:            http://github.com/lysxia/generic-random\nlicense:             MIT\nlicense-file:        LICENSE\nstability:           Stable\nauthor:              Li-yao Xia\nmaintainer:          lysxia@gmail.com\ncategory:            Generics, Testing\nbuild-type:          Simple\nextra-source-files:  README.md CHANGELOG.md\ncabal-version:       >=1.10\ntested-with:         GHC == 8.4.1, GHC == 8.6.1, GHC == 8.8.4, GHC == 8.10.5, GHC == 9.0.1, GHC == 9.2.1\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:\n    Generic.Random\n    Generic.Random.DerivingVia\n    Generic.Random.Internal.BaseCase\n    Generic.Random.Internal.Generic\n    Generic.Random.Tutorial\n  build-depends:\n    base >= 4.11 && < 5,\n    QuickCheck >= 2.14\n    -- exports RecursivelyShrink\n  default-language:    Haskell2010\n  ghc-options: -Wall -fno-warn-name-shadowing\n\nsource-repository head\n  type:     git\n  location: https://github.com/lysxia/generic-random\n\ntest-suite unit\n  hs-source-dirs:  test\n  main-is:         Unit.hs\n  build-depends:\n    base,\n    deepseq,\n    QuickCheck,\n    generic-random\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n\ntest-suite coherence\n  hs-source-dirs:  test\n  main-is:         coherence.hs\n  build-depends:\n    base,\n    deepseq,\n    QuickCheck,\n    generic-random\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n\ntest-suite inspect\n  hs-source-dirs:  test\n  main-is:         Inspect.hs\n  build-depends:\n    base,\n    QuickCheck,\n    inspection-testing,\n    generic-random\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n  if !flag(enable-inspect)\n    buildable: False\n  else\n    build-depends: random < 1.2\n    -- TODO: this test fails with newer versions of random\n\ntest-suite inspect-derivingvia\n  hs-source-dirs:  test\n  main-is:         Inspect/DerivingVia.hs\n  build-depends:\n    base,\n    QuickCheck,\n    inspection-testing,\n    generic-random\n  type: exitcode-stdio-1.0\n  default-language: Haskell2010\n  if !flag(enable-inspect)\n    buildable: False\n  else\n    build-depends: random < 1.2\n    -- TODO: this test fails with newer versions of random\n\nflag enable-inspect\n  description: Enable inspection tests\n  default: False\n  manual: True\n";
    }