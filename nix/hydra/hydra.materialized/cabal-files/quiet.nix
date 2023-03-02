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
      identifier = { name = "quiet"; version = "0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2016-2020";
      maintainer = "jacob@stanley.io";
      author = "Jacob Stanley";
      homepage = "https://github.com/jacobstanley/quiet#readme";
      url = "";
      synopsis = "Generic deriving of Read/Show with no record labels.";
      description = "Please see the README on GitHub at <https://github.com/jacobstanley/quiet#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "quiet-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quiet-0.2.tar.gz";
      sha256 = "118bf67379dce4737619998380e399acba306dc8a086a069d4a01d5694325e4c";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.31.2.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: bfd3822b32b35aee5958f968ae00d57da6247e3734721d63a91d5791126903ef\n\nname:           quiet\nversion:        0.2\nsynopsis:       Generic deriving of Read/Show with no record labels.\ndescription:    Please see the README on GitHub at <https://github.com/jacobstanley/quiet#readme>\ncategory:       Generics\nhomepage:       https://github.com/jacobstanley/quiet#readme\nbug-reports:    https://github.com/jacobstanley/quiet/issues\nauthor:         Jacob Stanley\nmaintainer:     jacob@stanley.io\ncopyright:      Copyright (c) 2016-2020\nlicense:        BSD3\nlicense-file:   LICENSE\ntested-with:    GHC==8.0.2,GHC==8.2.2,GHC==8.4.4,GHC==8.6.5,GHC==8.8.1\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/jacobstanley/quiet\n\nlibrary\n  exposed-modules:\n      Quiet\n      Quiet.Internal\n  other-modules:\n      Paths_quiet\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.7 && <5\n  default-language: Haskell2010\n\ntest-suite quiet-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Paths_quiet\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.7 && <5\n    , quiet\n  default-language: Haskell2010\n";
    }