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
      identifier = { name = "monad-supply"; version = "0.9"; };
      license = "LicenseRef-OtherLicense";
      copyright = "2011-2020 Geoff Hulette and HaskellWiki contributors";
      maintainer = "Geoff Hulette <geoff@hulette.net>";
      author = "Geoff Hulette and HaskellWiki contributors";
      homepage = "https://github.com/ghulette/monad-supply";
      url = "";
      synopsis = "Stateful supply monad";
      description = "Support for computations which consume values from a (possibly infinite) supply.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/monad-supply-0.9.tar.gz";
      sha256 = "4ae9723a91f14dee9cd080cd16e877b197ee65473ef0972dbe0e64c4d9bd76ca";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.33.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 39c53de256cb793f3b30bbe8f4c1d4e6bc458746ac18bbbb788ecb6cf7b7a265\n\nname:           monad-supply\nversion:        0.9\nsynopsis:       Stateful supply monad\ndescription:    Support for computations which consume values from a (possibly infinite) supply.\ncategory:       Control, Data, Monad\nstability:      experimental\nhomepage:       https://github.com/ghulette/monad-supply\nbug-reports:    https://github.com/ghulette/monad-supply/issues\nauthor:         Geoff Hulette and HaskellWiki contributors\nmaintainer:     Geoff Hulette <geoff@hulette.net>\ncopyright:      2011-2020 Geoff Hulette and HaskellWiki contributors\nlicense:        OtherLicense\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/ghulette/monad-supply\n\nlibrary\n  exposed-modules:\n      Control.Monad.Supply\n  other-modules:\n      Paths_monad_supply\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      base >=4.7 && <5\n    , mtl >=2.2.2 && <3.0.0\n    , transformers >=0.5.6.2 && <0.6.0.0\n  default-language: Haskell2010\n";
    }