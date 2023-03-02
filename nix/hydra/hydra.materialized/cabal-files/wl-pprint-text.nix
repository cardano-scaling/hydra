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
      identifier = { name = "wl-pprint-text"; version = "1.2.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2007 Daan Leijen, 2010 Ivan Lazar Miljenovic";
      maintainer = "Ivan.Miljenovic@gmail.com";
      author = "Ivan Lazar Miljenovic";
      homepage = "";
      url = "";
      synopsis = "A Wadler/Leijen Pretty Printer for Text values";
      description = "A clone of wl-pprint for use with the text library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wl-pprint-text-1.2.0.2.tar.gz";
      sha256 = "9215778d58ab9f71a4b8b5fb75c06438ff6ea7319a31eb6e97b4f67520dfb12b";
      });
    }) // {
    package-description-override = "Cabal-version:       >=1.10\nName:                wl-pprint-text\nVersion:             1.2.0.2\nSynopsis:            A Wadler/Leijen Pretty Printer for Text values\nDescription:         A clone of wl-pprint for use with the text library.\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Ivan Lazar Miljenovic\nMaintainer:          Ivan.Miljenovic@gmail.com\nCopyright:           2007 Daan Leijen, 2010 Ivan Lazar Miljenovic\nCategory:            Text\nBuild-type:          Simple\n\nTested-With:\n    GHC == 7.4.2\n    GHC == 7.6.3\n    GHC == 7.8.4\n    GHC == 7.10.3\n    GHC == 8.0.2\n    GHC == 8.2.2\n    GHC == 8.4.4\n    GHC == 8.6.5\n    GHC == 8.8.4\n    GHC == 8.10.7\n    GHC == 9.0.1\n    GHC == 9.2.1\n\nExtra-Source-Files: Changelog.md\n                    README.md\n\nSource-Repository head\n    type: git\n    location: https://github.com/ivan-m/wl-pprint-text.git\n\nLibrary\n  Exposed-modules:     Text.PrettyPrint.Leijen.Text,\n                       Text.PrettyPrint.Leijen.Text.Monadic\n  Build-depends:       base        >= 4.5.0.0  && < 5,\n                       base-compat >= 0.10     && < 0.13,\n                       text        >= 0.11.0.0 && < 2.1\n  Default-language:    Haskell98\n  GHC-Options:         -Wall\n";
    }