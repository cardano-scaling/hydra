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
      identifier = { name = "ekg-json"; version = "0.1.0.7.0.0.0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Johan Tibell <johan.tibell@gmail.com>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>";
      author = "Johan Tibell";
      homepage = "https://github.com/tibbe/ekg-json";
      url = "";
      synopsis = "JSON encoding of ekg metrics";
      description = "Encodes ekg metrics as JSON, using the same encoding as used by the\nekg package, thus allowing ekg metrics to be served by other HTTP\nservers than the one used by the ekg package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/ekg-json-0.1.0.7.0.0.0.0.1.tar.gz";
      sha256 = "6031a2f5f1570606c5fedf32dc40997f00d6b3a2f762f49f0870adbd5e36bb7e";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               ekg-json\nversion:            0.1.0.7.0.0.0.0.1\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:\n    Johan Tibell <johan.tibell@gmail.com>,\n    Mikhail Glushenkov <mikhail.glushenkov@gmail.com>\n\nauthor:             Johan Tibell\ntested-with:\n    ghc ==8.10.1 ghc ==8.8.3 ghc ==8.6.5 ghc ==8.4.4 ghc ==8.2.2\n    ghc ==8.0.2 ghc ==7.10.3 ghc ==7.8.4 ghc ==7.6.3\n\nhomepage:           https://github.com/tibbe/ekg-json\nbug-reports:        https://github.com/tibbe/ekg-json/issues\nsynopsis:           JSON encoding of ekg metrics\ndescription:\n    Encodes ekg metrics as JSON, using the same encoding as used by the\n    ekg package, thus allowing ekg metrics to be served by other HTTP\n    servers than the one used by the ekg package.\n\ncategory:           Distribution\nbuild-type:         Simple\nextra-source-files: CHANGES.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/tibbe/ekg-json.git\n\nlibrary\n    exposed-modules:  System.Metrics.Json\n    default-language: Haskell2010\n    build-depends:\n        aeson >=2.0 && <2.1,\n        base >=4.6 && <4.17,\n        ekg-core >=0.1 && <0.2,\n        text <1.3 || >=2.0 && <2.1,\n        unordered-containers <0.3\n";
    }