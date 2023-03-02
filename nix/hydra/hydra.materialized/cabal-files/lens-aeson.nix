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
      identifier = { name = "lens-aeson"; version = "1.2.2"; };
      license = "MIT";
      copyright = "Copyright (C) 2012 Paul Wilson\nCopyright (C) 2013 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/lens/lens-aeson/";
      url = "";
      synopsis = "Law-abiding lenses for aeson";
      description = "Law-abiding lenses for aeson.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lens-aeson-1.2.2.tar.gz";
      sha256 = "14d13885b47fd9448a8f2019eda600b4556c784b02d1b86a6dc114b13f789573";
      });
    }) // {
    package-description-override = "name:          lens-aeson\ncategory:      Data, JSON, Lenses\nversion:       1.2.2\nlicense:       MIT\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/lens/lens-aeson/\nbug-reports:   http://github.com/lens/lens-aeson/issues\ncopyright:\n  Copyright (C) 2012 Paul Wilson\n  Copyright (C) 2013 Edward A. Kmett\nbuild-type:    Simple\ntested-with:   GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.7\n             , GHC == 9.0.2\n             , GHC == 9.2.2\nsynopsis:      Law-abiding lenses for aeson\ndescription:   Law-abiding lenses for aeson.\n\nextra-source-files:\n  .gitignore\n  .hlint.yaml\n  .vim.custom\n  AUTHORS.markdown\n  README.markdown\n  CHANGELOG.markdown\n\nsource-repository head\n  type: git\n  location: https://github.com/lens/lens-aeson\n\nlibrary\n  build-depends:\n    base                 >= 4.9       && < 5,\n    lens                 >= 5.0       && < 6,\n    text                 >= 0.11.1.10 && < 2.1,\n    text-short           >= 0.1.4     && < 0.2,\n    vector               >= 0.9       && < 0.14,\n    unordered-containers >= 0.2.3     && < 0.3,\n    attoparsec           >= 0.10      && < 0.15,\n    bytestring           >= 0.9       && < 0.12,\n    aeson                >= 2.0.2     && < 2.2,\n    scientific           >= 0.3.2     && < 0.4\n\n  exposed-modules:\n    Data.Aeson.Lens\n\n  ghc-options: -Wall -Wtabs -O2\n  hs-source-dirs: src\n  default-language: Haskell2010\n";
    }