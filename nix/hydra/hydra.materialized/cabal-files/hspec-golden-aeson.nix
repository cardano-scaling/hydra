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
      identifier = { name = "hspec-golden-aeson"; version = "0.9.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2016 Plow Technologies";
      maintainer = "mchaver@gmail.com";
      author = "James M.C. Haver II";
      homepage = "https://github.com/plow-technologies/hspec-golden-aeson#readme";
      url = "";
      synopsis = "Use tests to monitor changes in Aeson serialization";
      description = "Use tests to monitor changes in Aeson serialization";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."quickcheck-arbitrary-adt" or (errorHandler.buildDepError "quickcheck-arbitrary-adt"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-golden-aeson" or (errorHandler.buildDepError "hspec-golden-aeson"))
            (hsPkgs."quickcheck-arbitrary-adt" or (errorHandler.buildDepError "quickcheck-arbitrary-adt"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-golden-aeson-0.9.0.0.tar.gz";
      sha256 = "04b1510fd21af7ccefbb753ffeb6474eed055120a27ef7fcbcc22a63bb0945d6";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.33.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 11e24a472a71971f598d5ce65cdafa167658b0e163868ee1de71561acdc8301f\n\nname:           hspec-golden-aeson\nversion:        0.9.0.0\nsynopsis:       Use tests to monitor changes in Aeson serialization\ndescription:    Use tests to monitor changes in Aeson serialization\ncategory:       Testing\nhomepage:       https://github.com/plow-technologies/hspec-golden-aeson#readme\nbug-reports:    https://github.com/plow-technologies/hspec-golden-aeson/issues\nauthor:         James M.C. Haver II\nmaintainer:     mchaver@gmail.com\ncopyright:      2016 Plow Technologies\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n  ChangeLog.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/plow-technologies/hspec-golden-aeson\n\nlibrary\n  exposed-modules:\n      Test.Aeson.GenericSpecs\n      Test.Aeson.Internal.ADT.GoldenSpecs\n      Test.Aeson.Internal.ADT.RoundtripSpecs\n      Test.Aeson.Internal.GoldenSpecs\n      Test.Aeson.Internal.RoundtripSpecs\n      Test.Aeson.Internal.RandomSamples\n      Test.Aeson.Internal.Utils\n  other-modules:\n      Paths_hspec_golden_aeson\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      HUnit\n    , QuickCheck\n    , aeson\n    , aeson-pretty\n    , base >=4.7 && <5\n    , bytestring\n    , directory\n    , filepath\n    , hspec\n    , quickcheck-arbitrary-adt >=0.3.0.0\n    , random\n    , transformers\n  default-language: Haskell2010\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Test.Aeson.GenericSpecsSpec\n      Test.Types\n      Test.Types.AlteredSelector\n      Test.Types.BrokenSerialization\n      Test.Types.MismatchedToAndFromSerialization\n      Test.Types.NewSelector\n      Test.Utils\n      Paths_hspec_golden_aeson\n  hs-source-dirs:\n      test\n  ghc-options: -Wall\n  build-depends:\n      QuickCheck\n    , aeson\n    , base\n    , directory\n    , hspec\n    , hspec-core\n    , hspec-golden-aeson\n    , quickcheck-arbitrary-adt\n    , silently\n    , transformers\n  default-language: Haskell2010\n";
    }