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
      identifier = { name = "microlens-mtl"; version = "0.2.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Monadfix <hi@monadfix.com>";
      author = "Edward Kmett, Artyom Kazak";
      homepage = "http://github.com/monadfix/microlens";
      url = "";
      synopsis = "microlens support for Reader/Writer/State from mtl";
      description = "This package contains functions (like 'view' or '+=') which work on 'MonadReader', 'MonadWriter', and 'MonadState' from the mtl package.\n\nThis package is a part of the <http://hackage.haskell.org/package/microlens microlens> family; see the readme <https://github.com/monadfix/microlens#readme on Github>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/microlens-mtl-0.2.0.2.tar.gz";
      sha256 = "66971668f376717fdbe75e3b8ac99d2efd89c746b41355f6d388a63352a43278";
      });
    }) // {
    package-description-override = "name:                microlens-mtl\nversion:             0.2.0.2\nsynopsis:            microlens support for Reader/Writer/State from mtl\ndescription:\n  This package contains functions (like 'view' or '+=') which work on 'MonadReader', 'MonadWriter', and 'MonadState' from the mtl package.\n  .\n  This package is a part of the <http://hackage.haskell.org/package/microlens microlens> family; see the readme <https://github.com/monadfix/microlens#readme on Github>.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Edward Kmett, Artyom Kazak\nmaintainer:          Monadfix <hi@monadfix.com>\nhomepage:            http://github.com/monadfix/microlens\nbug-reports:         http://github.com/monadfix/microlens/issues\n-- copyright:\ncategory:            Data, Lenses\nbuild-type:          Simple\nextra-source-files:\n  CHANGELOG.md\ncabal-version:       >=1.10\ntested-with:         GHC==7.6.3\n                     GHC==7.8.4\n                     GHC==7.10.3\n                     GHC==8.0.2\n                     GHC==8.2.2\n                     GHC==8.4.4\n                     GHC==8.6.5\n                     GHC==8.8.4\n                     GHC==8.10.7\n                     GHC==9.0.1\n                     GHC==9.2.1\n\nsource-repository head\n  type:                git\n  location:            git://github.com/monadfix/microlens.git\n\nlibrary\n  exposed-modules:     Lens.Micro.Mtl\n                       Lens.Micro.Mtl.Internal\n  -- other-extensions:\n  build-depends:       base >=4.5 && <5\n                     , microlens >=0.4 && <0.5\n                     , mtl >=2.0.1 && <2.4\n                     , transformers >=0.2 && <0.7\n                     , transformers-compat >=0.4 && <1\n\n  ghc-options:\n    -Wall -fwarn-tabs\n    -O2 -fdicts-cheap -funbox-strict-fields\n    -fmax-simplifier-iterations=10\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n";
    }