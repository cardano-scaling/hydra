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
    flags = { bytestring = true; text = true; vector = true; };
    package = {
      specVersion = "2.4";
      identifier = { name = "nothunks"; version = "0.1.3"; };
      license = "MIT";
      copyright = "2018-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Examine values for unexpected thunks";
      description = "Long lived application data typically should not contain\nany thunks. This library can be used to examine values for\nunexpected thunks, which can then be used in assertions.\nThis can be invaluable in avoiding memory leaks, or tracking\ndown existing ones.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
          ] ++ (pkgs.lib).optional (flags.bytestring) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ (pkgs.lib).optional (flags.vector) (hsPkgs."vector" or (errorHandler.buildDepError "vector"));
        buildable = true;
        };
      tests = {
        "nothunks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nothunks-0.1.3.tar.gz";
      sha256 = "c191e9c3d86ca006bf8d85adfd936ff997759120b0fcfaf4960a56d8bd850e53";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\nname:               nothunks\nversion:            0.1.3\nsynopsis:           Examine values for unexpected thunks\ndescription:        Long lived application data typically should not contain\n                    any thunks. This library can be used to examine values for\n                    unexpected thunks, which can then be used in assertions.\n                    This can be invaluable in avoiding memory leaks, or tracking\n                    down existing ones.\nlicense:            MIT\nlicense-file:       LICENSE\nbug-reports:        https://github.com/input-output-hk/nothunks\nauthor:             IOHK\nmaintainer:         operations@iohk.io\ncopyright:          2018-2021 IOHK\ncategory:           Development\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/nothunks\n\nflag bytestring\n  description: Provide instances for bytestring\n  default: True\n\nflag text\n  description: Provide instances for text\n  default: True\n\nflag vector\n  description: Provide instances for vector\n  default: True\n\nlibrary\n    exposed-modules:  NoThunks.Class\n\n    build-depends:    base       >= 4.12 && < 5\n                    , containers >= 0.5  && < 0.7\n                    , stm        >= 2.5  && < 2.6\n                    , time       >= 1.5  && < 1.13\n\n                      -- Whatever is bundled with ghc\n                    , ghc-heap\n\n    if flag(bytestring)\n      build-depends:  bytestring >= 0.10 && < 0.12\n    if flag(text)\n      build-depends:  text       >= 1.2  && < 1.3\n    if flag(vector)\n      build-depends:  vector     >= 0.12 && < 0.13\n\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    ghc-options:      -Wall\n\ntest-suite nothunks-test\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    other-modules:    Test.NoThunks.Class\n\n    build-depends:    base\n\n                      -- Self dependency\n                    , nothunks\n\n                      -- Dependencies shared with the lib\n                    , containers\n                    , stm\n\n                      -- Whatever is bundled with ghc\n                    , ghc-prim\n\n                      -- Additional dependencies\n                    , hedgehog       >= 1.0 && < 1.1\n                    , random         >= 1.1 && < 1.3\n                    , tasty          >= 1.3 && < 1.5\n                    , tasty-hedgehog >= 1.0 && < 1.2\n\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -Wall\n";
    }