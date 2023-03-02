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
      specVersion = "1.8";
      identifier = { name = "ekg"; version = "0.4.0.15"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Johan Tibell <johan.tibell@gmail.com>,\nMikhail Glushenkov <mikhail.glushenkov@gmail.com>";
      author = "Johan Tibell";
      homepage = "https://github.com/tibbe/ekg";
      url = "";
      synopsis = "Remote monitoring of processes";
      description = "This library lets you remotely monitor a running process over HTTP.\nIt provides a simple way to integrate a monitoring server into any\napplication.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."ekg-json" or (errorHandler.buildDepError "ekg-json"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
          (hsPkgs."snap-server" or (errorHandler.buildDepError "snap-server"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ekg-0.4.0.15.tar.gz";
      sha256 = "482ae3be495cfe4f03332ad1c79ce8b5ad4f9c8eec824980c664808ae32c6dcc";
      });
    }) // {
    package-description-override = "name:                ekg\r\nversion:             0.4.0.15\r\nx-revision: 8\r\ncabal-version:       >= 1.8\r\nsynopsis:            Remote monitoring of processes\r\ndescription:\r\n  This library lets you remotely monitor a running process over HTTP.\r\n  It provides a simple way to integrate a monitoring server into any\r\n  application.\r\nhomepage:            https://github.com/tibbe/ekg\r\nbug-reports:         https://github.com/tibbe/ekg/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Johan Tibell\r\nmaintainer:          Johan Tibell <johan.tibell@gmail.com>,\r\n                     Mikhail Glushenkov <mikhail.glushenkov@gmail.com>\r\ncategory:            System, Network\r\nbuild-type:          Simple\r\ndata-files:          assets/index.html assets/monitor.js assets/monitor.css\r\n                     assets/jquery.flot.min.js assets/jquery-1.6.4.min.js\r\n                     assets/bootstrap-1.4.0.min.css\r\n                     assets/chart_line_add.png assets/cross.png\r\nextra-source-files:  LICENSE.icons LICENSE.javascript README.md\r\n                     assets/jquery-1.6.4.js assets/jquery.flot.js\r\n                     assets/bootstrap-1.4.0.css\r\n                     examples/Basic.hs CHANGES.md\r\ntested-with:         GHC == 8.4.1,  GHC == 8.2.2, GHC == 8.0.2,\r\n                     GHC == 7.10.3, GHC == 7.8.4, GHC == 7.6.3\r\n\r\nlibrary\r\n  exposed-modules:\r\n    System.Remote.Counter\r\n    System.Remote.Gauge\r\n    System.Remote.Label\r\n    System.Remote.Monitoring\r\n\r\n  other-modules:\r\n    Paths_ekg\r\n    System.Remote.Json\r\n    System.Remote.Snap\r\n\r\n  build-depends:\r\n    aeson >= 0.4 && < 1.6,\r\n    base >= 4.5 && < 4.15,\r\n    bytestring < 1.0,\r\n    ekg-core >= 0.1 && < 0.2,\r\n    ekg-json >= 0.1 && < 0.2,\r\n    filepath < 1.5,\r\n    network < 3.2,\r\n    snap-core < 1.1,\r\n    snap-server < 1.2,\r\n    text < 1.3,\r\n    time < 1.10,\r\n    transformers < 0.6,\r\n    unordered-containers < 0.3\r\n\r\n  ghc-options: -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/tibbe/ekg.git\r\n";
    }