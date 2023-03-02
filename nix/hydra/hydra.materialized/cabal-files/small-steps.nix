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
    flags = { sts_assert = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "small-steps"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."validation-selective" or (errorHandler.buildDepError "validation-selective"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/small-steps-0.1.0.0.tar.gz";
      sha256 = "de9ce9f4cfcca2312945796549ebf40174042a7b892111878b1340c20cefe04f";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                small-steps\nversion:             0.1.0.0\nsynopsis:            Small step semantics\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncategory:            Control\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   libs/small-steps\n\nflag sts_assert\n    description: Enable STS assertions by default\n    default: False\n    manual: True\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:     Control.State.Transition\n                     , Control.State.Transition.Extended\n                     , Control.State.Transition.Simple\n                     , Control.Provenance\n\n  build-depends:       aeson\n                     , base >=4.11 && <5\n                     , containers\n                     , data-default-class\n                     , free\n                     , mtl\n                     , nothunks\n                     , strict-containers\n                     , text\n                     , transformers >= 0.5\n                     , validation-selective\n\n  hs-source-dirs:      src\n  if (flag(sts_assert))\n    cpp-options:      -DSTS_ASSERT\n";
    }