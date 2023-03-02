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
    flags = { lib-werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "retry"; version = "0.9.3.0"; };
      license = "BSD-3-Clause";
      copyright = "Ozgun Ataman, Soostone Inc";
      maintainer = "ozgun.ataman@soostone.com";
      author = "Ozgun Ataman";
      homepage = "http://github.com/Soostone/retry";
      url = "";
      synopsis = "Retry combinators for monadic actions that may fail";
      description = "This package exposes combinators that can wrap arbitrary\nmonadic actions. They run the action and potentially retry\nrunning it with some configurable delay for a configurable\nnumber of times.\nThe purpose is to make it easier to work with IO and\nespecially network IO actions that often experience temporary\nfailure and warrant retrying of the original action. For\nexample, a database query may time out for a while, in which\ncase we should hang back for a bit and retry the query instead\nof simply raising an exception.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mtl-compat" or (errorHandler.buildDepError "mtl-compat"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."mtl-compat" or (errorHandler.buildDepError "mtl-compat"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/retry-0.9.3.0.tar.gz";
      sha256 = "c11e5dcb6fc9b7a327ebdf9188edcbe39fce64e728294e41a61e9ad94fa84ecd";
      });
    }) // {
    package-description-override = "name:                retry\n\ndescription:\n\n        This package exposes combinators that can wrap arbitrary\n        monadic actions. They run the action and potentially retry\n        running it with some configurable delay for a configurable\n        number of times.\n\n        The purpose is to make it easier to work with IO and\n        especially network IO actions that often experience temporary\n        failure and warrant retrying of the original action. For\n        example, a database query may time out for a while, in which\n        case we should hang back for a bit and retry the query instead\n        of simply raising an exception.\n\nversion:             0.9.3.0\nsynopsis:            Retry combinators for monadic actions that may fail\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ozgun Ataman\nmaintainer:          ozgun.ataman@soostone.com\ncopyright:           Ozgun Ataman, Soostone Inc\ncategory:            Control\nbuild-type:          Simple\ncabal-version:       >=1.10\nhomepage:            http://github.com/Soostone/retry\nextra-source-files:\n  README.md\n  changelog.md\n\nflag lib-Werror\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:     Control.Retry\n                       UnliftIO.Retry\n  build-depends:\n      base                 >= 4.8 && < 5\n    , exceptions           >= 0.5\n    , ghc-prim\n    , random               >= 1\n    , transformers\n    , mtl\n    , mtl-compat\n    , unliftio-core        >= 0.1.0.0\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n\n  if flag(lib-Werror)\n    ghc-options: -Werror\n\n  ghc-options: -Wall\n\n\ntest-suite test\n    type:           exitcode-stdio-1.0\n    main-is:        Main.hs\n    hs-source-dirs: test,src\n    ghc-options:    -threaded\n    other-modules:  Control.Retry\n                    UnliftIO.Retry\n                    Tests.Control.Retry\n                    Tests.UnliftIO.Retry\n    build-depends:\n        base              ==4.*\n      , exceptions\n      , transformers\n      , random\n      , time\n      , HUnit              >= 1.2.5.2\n      , tasty\n      , tasty-hunit\n      , tasty-hedgehog\n      , hedgehog           >= 1.0\n      , stm\n      , ghc-prim\n      , mtl\n      , mtl-compat\n      , unliftio-core\n    default-language: Haskell2010\n\n    if flag(lib-Werror)\n      ghc-options: -Werror\n\n    ghc-options: -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/Soostone/retry.git\n";
    }