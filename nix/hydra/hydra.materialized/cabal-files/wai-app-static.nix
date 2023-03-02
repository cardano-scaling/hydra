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
    flags = { print = false; cryptonite = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "wai-app-static"; version = "3.1.7.4"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/web-application-interface";
      url = "";
      synopsis = "WAI application for static serving";
      description = "API docs and the README are available at <http://www.stackage.org/package/wai-app-static>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ] ++ (if flags.cryptonite
          then [
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            ]
          else [
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."cryptohash-md5" or (errorHandler.buildDepError "cryptohash-md5"))
            ]);
        buildable = true;
        };
      exes = {
        "warp" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            ];
          buildable = true;
          };
        };
      tests = {
        "runtests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-app-static-3.1.7.4.tar.gz";
      sha256 = "647188444b19dc953bbfbab5611c81c1e1f27c31bac5dc31dd5de27cdbf01fc1";
      });
    }) // {
    package-description-override = "name:            wai-app-static\nversion:         3.1.7.4\nlicense:         MIT\nlicense-file:    LICENSE\nauthor:          Michael Snoyman <michael@snoyman.com>\nmaintainer:      Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>\nsynopsis:        WAI application for static serving\ndescription:     API docs and the README are available at <http://www.stackage.org/package/wai-app-static>.\ncategory:        Web, Yesod\nstability:       Stable\ncabal-version:   >= 1.10\nbuild-type:      Simple\nhomepage:        http://www.yesodweb.com/book/web-application-interface\nExtra-source-files:\n  images/folder.png\n  images/haskell.png\n  test/*.hs\n  test/a/b\n  tests.hs\n  README.md\n  ChangeLog.md\n\nFlag print\n    Description:   print debug info\n    Default:       False\n\nFlag cryptonite\n    Description:   Use the cryptonite library for MD5 computation\n    Default:       True\n\nlibrary\n    default-language: Haskell2010\n    build-depends:   base                      >= 4.12     && < 5\n                   , wai                       >= 3.0      && < 3.3\n                   , bytestring                >= 0.10.4\n                   , http-types                >= 0.7\n                   , transformers              >= 0.2.2\n                   , unix-compat               >= 0.2\n                   , directory                 >= 1.0.1\n                   , containers                >= 0.2\n                   , time                      >= 1.1.4\n                   , old-locale                >= 1.0.0.2\n                   , file-embed                >= 0.0.3.1\n                   , text                      >= 0.7\n                   , http-date\n                   , blaze-html                >= 0.5\n                   , blaze-markup              >= 0.5.1\n                   , mime-types                >= 0.1      && < 0.2\n                   , unordered-containers      >= 0.2\n                   , template-haskell          >= 2.7\n                   , zlib                      >= 0.5\n                   , filepath\n                   , wai-extra                 >= 3.0      && < 3.2\n                   , optparse-applicative      >= 0.7\n                   , warp                      >= 3.0.11   && < 3.4\n    if flag(cryptonite)\n      build-depends: cryptonite                >= 0.6\n                   , memory                    >= 0.7\n    else\n      build-depends: base64-bytestring         >= 0.1\n                   , cryptohash-md5            >= 0.11.101\n\n    exposed-modules: Network.Wai.Application.Static\n                     WaiAppStatic.Storage.Filesystem\n                     WaiAppStatic.Storage.Embedded\n                     WaiAppStatic.Listing\n                     WaiAppStatic.Types\n                     WaiAppStatic.CmdLine\n    other-modules:   Util\n                     WaiAppStatic.Storage.Embedded.Runtime\n                     WaiAppStatic.Storage.Embedded.TH\n    ghc-options:     -Wall\n\n    if flag(print)\n      cpp-options:  -DPRINT\n\nExecutable warp\n  default-language: Haskell2010\n  Main-is:        warp-static.hs\n  hs-source-dirs: app\n  Build-depends: base            >= 4                  && < 5\n               , wai-app-static\n               , directory       >= 1.0\n               , containers      >= 0.2\n               , bytestring      >= 0.10.4\n               , text            >= 0.7\n               , mime-types      >= 0.1                && < 0.2\n\ntest-suite runtests\n    default-language: Haskell2010\n    hs-source-dirs: test\n    main-is: ../tests.hs\n    type: exitcode-stdio-1.0\n\n    build-depends:   base                      >= 4        && < 5\n                   , hspec                     >= 1.3\n                   , unix-compat\n                   , time\n                   , old-locale\n                   , http-date\n                   , wai-app-static\n                   , wai-extra\n                   , wai\n                   , http-types\n                   , network\n                   , bytestring\n                   , text\n                   , transformers\n                   , mime-types\n                   , zlib\n                   , filepath\n                   , temporary\n                   , mockery\n                   -- , containers\n    ghc-options:   -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }