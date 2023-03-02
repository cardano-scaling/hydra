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
      identifier = { name = "zlib-bindings"; version = "0.1.1.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Gregory Collins <greg@gregorycollins.net>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://github.com/snapframework/zlib-bindings";
      url = "";
      synopsis = "Low-level bindings to the zlib package.";
      description = "Low-level bindings to the zlib package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."zlib-bindings" or (errorHandler.buildDepError "zlib-bindings"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zlib-bindings-0.1.1.5.tar.gz";
      sha256 = "c83bb438f9b6c5fe860982731eb8ac7eff993e8b56cbc15ef5b471f229f79109";
      });
    }) // {
    package-description-override = "name:            zlib-bindings\r\nversion:         0.1.1.5\r\nx-revision: 2\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Michael Snoyman <michael@snoyman.com>\r\nmaintainer:      Gregory Collins <greg@gregorycollins.net>\r\nsynopsis:        Low-level bindings to the zlib package.\r\ndescription:     Low-level bindings to the zlib package.\r\ncategory:        Codec\r\nstability:       Experimental\r\ncabal-version:   >= 1.8\r\nbuild-type:      Simple\r\nhomepage:        http://github.com/snapframework/zlib-bindings\r\nextra-source-files: cbits/crc32.h cbits/inffast.h cbits/inflate.h\r\n                    cbits/trees.h cbits/deflate.h cbits/inffixed.h\r\n                    cbits/inftrees.h cbits/zutil.h, test/main.hs,\r\n                    LICENSE.gz\r\n\r\nlibrary\r\n    build-depends: base                  >= 4       && < 5\r\n                 , bytestring            >= 0.9.1.4\r\n                 , zlib                  >= 0.5.2.0\r\n    exposed-modules: Codec.Zlib\r\n                     Codec.Zlib.Lowlevel\r\n    ghc-options:     -Wall\r\n    c-sources:       c/helper.c\r\n    if os(windows)\r\n        include-dirs:  cbits\r\n        install-includes: zlib.h zconf.h\r\n\r\ntest-suite test \r\n    main-is: main.hs\r\n    hs-source-dirs: test\r\n    type: exitcode-stdio-1.0\r\n    build-depends: base                  >= 4       && < 5\r\n                 , bytestring\r\n                 , zlib\r\n                 , zlib-bindings\r\n                 , hspec                 >= 1.3\r\n                 , QuickCheck            >= 2.3\r\n    ghc-options:     -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snapframework/zlib-bindings.git\r\n";
    }