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
      identifier = { name = "xml-conduit"; version = "1.9.1.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Aristid Breitkreuz <aristidb@googlemail.com>";
      homepage = "http://github.com/snoyberg/xml";
      url = "";
      synopsis = "Pure-Haskell utilities for dealing with XML with the conduit package.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/xml-conduit>.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."xml-types" or (errorHandler.buildDepError "xml-types"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."xml-types" or (errorHandler.buildDepError "xml-types"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            ];
          buildable = true;
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/xml-conduit-1.9.1.1.tar.gz";
      sha256 = "bdb117606c0b56ca735564465b14b50f77f84c9e52e31d966ac8d4556d3ff0ff";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.14\r\n\r\nname:            xml-conduit\r\nversion:         1.9.1.1\r\nx-revision: 1\r\nlicense:         MIT\r\nlicense-file:    LICENSE\r\nauthor:          Michael Snoyman <michael@snoyman.com>, Aristid Breitkreuz <aristidb@googlemail.com>\r\nmaintainer:      Michael Snoyman <michael@snoyman.com>\r\nsynopsis:        Pure-Haskell utilities for dealing with XML with the conduit package.\r\ndescription:     Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/xml-conduit>.\r\ncategory:        XML, Conduit\r\nstability:       Stable\r\nbuild-type:      Custom\r\nhomepage:        http://github.com/snoyberg/xml\r\nextra-source-files: README.md\r\n                    ChangeLog.md\r\ntested-with:     GHC >=8.0 && <8.12\r\n\r\ncustom-setup\r\n    setup-depends:   base >= 4 && <5, Cabal, cabal-doctest >= 1 && <1.1\r\n\r\nlibrary\r\n    build-depends:   base                      >= 4        && < 5\r\n                   , conduit                   >= 1.3      && < 1.4\r\n                   , conduit-extra             >= 1.3      && < 1.4\r\n                   , resourcet                 >= 1.2      && < 1.3\r\n                   , bytestring                >= 0.10.2\r\n                   , text                      >= 0.7\r\n                   , containers                >= 0.2\r\n                   , xml-types                 >= 0.3.4    && < 0.4\r\n                   , attoparsec                >= 0.10\r\n                   , transformers              >= 0.2      && < 0.7\r\n                   , data-default-class\r\n                   , blaze-markup              >= 0.5\r\n                   , blaze-html                >= 0.5\r\n                   , deepseq                   >= 1.1.0.0\r\n    exposed-modules: Text.XML.Stream.Parse\r\n                     Text.XML.Stream.Render\r\n                     Text.XML.Unresolved\r\n                     Text.XML.Cursor\r\n                     Text.XML.Cursor.Generic\r\n                     Text.XML\r\n    other-modules:   Text.XML.Stream.Token\r\n    ghc-options:     -Wall\r\n    hs-source-dirs:  src\r\n    default-language: Haskell2010\r\n\r\ntest-suite unit\r\n    type: exitcode-stdio-1.0\r\n    main-is: unit.hs\r\n    hs-source-dirs: test\r\n    build-depends:          base\r\n                          , containers\r\n                          , text\r\n                          , transformers\r\n                          , bytestring\r\n                          , xml-conduit\r\n                          , hspec >= 1.3\r\n                          , HUnit\r\n                          , xml-types >= 0.3.1\r\n                          , conduit\r\n                          , conduit-extra\r\n                          , blaze-markup\r\n                          , resourcet\r\n    default-language: Haskell2010\r\n\r\ntest-suite doctest\r\n    type: exitcode-stdio-1.0\r\n    main-is: doctest.hs\r\n    hs-source-dirs: test\r\n    build-depends:          base\r\n                          , doctest >= 0.8\r\n                          , xml-conduit\r\n    default-language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snoyberg/xml.git\r\n";
    }