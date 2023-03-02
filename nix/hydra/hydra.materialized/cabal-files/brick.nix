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
    flags = { demos = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "brick"; version = "0.73"; };
      license = "BSD-3-Clause";
      copyright = "(c) Jonathan Daugherty 2015-2022";
      maintainer = "Jonathan Daugherty <cygnus@foobox.com>";
      author = "Jonathan Daugherty <cygnus@foobox.com>";
      homepage = "https://github.com/jtdaugherty/brick/";
      url = "";
      synopsis = "A declarative terminal user interface library";
      description = "Write terminal user interfaces (TUIs) painlessly with 'brick'! You\nwrite an event handler and a drawing function and the library does the\nrest.\n\n\n> module Main where\n>\n> import Brick\n>\n> ui :: Widget ()\n> ui = str \"Hello, world!\"\n>\n> main :: IO ()\n> main = simpleMain ui\n\n\nTo get started, see:\n\n* <https://github.com/jtdaugherty/brick/blob/master/README.md The README>\n\n* The <https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst Brick user guide>\n\n* The demonstration programs in the 'programs' directory\n\n\nThis package deprecates <http://hackage.haskell.org/package/vty-ui vty-ui>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."data-clist" or (errorHandler.buildDepError "data-clist"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
          (hsPkgs."config-ini" or (errorHandler.buildDepError "config-ini"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-zipper" or (errorHandler.buildDepError "text-zipper"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."word-wrap" or (errorHandler.buildDepError "word-wrap"))
          ];
        buildable = true;
        };
      exes = {
        "brick-table-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-tail-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-readme-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-file-browser-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-form-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-text-wrap-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."word-wrap" or (errorHandler.buildDepError "word-wrap"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-cache-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-visibility-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-viewport-scrollbars-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-viewport-scroll-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-dialog-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-mouse-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."text-zipper" or (errorHandler.buildDepError "text-zipper"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-layer-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-suspend-resume-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-cropping-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-padding-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-theme-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-attr-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-list-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-list-vi-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-custom-event-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-fill-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-hello-world-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-edit-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-border-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-dynamic-border-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "brick-progressbar-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            ];
          buildable = if !flags.demos then false else true;
          };
        };
      tests = {
        "brick-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/brick-0.73.tar.gz";
      sha256 = "741c8d0717f0ab5addd5d3acc88cb36d645a0c73907bde509b2fd9d9bc02039c";
      });
    }) // {
    package-description-override = "name:                brick\nversion:             0.73\nsynopsis:            A declarative terminal user interface library\ndescription:\n  Write terminal user interfaces (TUIs) painlessly with 'brick'! You\n  write an event handler and a drawing function and the library does the\n  rest.\n  .\n  .\n  > module Main where\n  >\n  > import Brick\n  >\n  > ui :: Widget ()\n  > ui = str \"Hello, world!\"\n  >\n  > main :: IO ()\n  > main = simpleMain ui\n  .\n  .\n  To get started, see:\n  .\n  * <https://github.com/jtdaugherty/brick/blob/master/README.md The README>\n  .\n  * The <https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst Brick user guide>\n  .\n  * The demonstration programs in the 'programs' directory\n  .\n  .\n  This package deprecates <http://hackage.haskell.org/package/vty-ui vty-ui>.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Jonathan Daugherty <cygnus@foobox.com>\nmaintainer:          Jonathan Daugherty <cygnus@foobox.com>\ncopyright:           (c) Jonathan Daugherty 2015-2022\ncategory:            Graphics\nbuild-type:          Simple\ncabal-version:       1.18\nHomepage:            https://github.com/jtdaugherty/brick/\nBug-reports:         https://github.com/jtdaugherty/brick/issues\ntested-with:         GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.1\n\nextra-doc-files:     README.md,\n                     docs/guide.rst,\n                     docs/samtay-tutorial.md,\n                     docs/snake-demo.gif,\n                     CHANGELOG.md,\n                     docs/programs-screenshots.md,\n                     docs/programs-screenshots/brick-attr-demo.png,\n                     docs/programs-screenshots/brick-border-demo.png,\n                     docs/programs-screenshots/brick-cache-demo.png,\n                     docs/programs-screenshots/brick-custom-event-demo.png,\n                     docs/programs-screenshots/brick-dialog-demo.png,\n                     docs/programs-screenshots/brick-dynamic-border-demo.png,\n                     docs/programs-screenshots/brick-edit-demo.png,\n                     docs/programs-screenshots/brick-file-browser-demo.png,\n                     docs/programs-screenshots/brick-fill-demo.png,\n                     docs/programs-screenshots/brick-form-demo.png,\n                     docs/programs-screenshots/brick-hello-world-demo.png,\n                     docs/programs-screenshots/brick-layer-demo.png,\n                     docs/programs-screenshots/brick-list-demo.png,\n                     docs/programs-screenshots/brick-list-vi-demo.png,\n                     docs/programs-screenshots/brick-mouse-demo.png,\n                     docs/programs-screenshots/brick-padding-demo.png,\n                     docs/programs-screenshots/brick-progressbar-demo.png,\n                     docs/programs-screenshots/brick-readme-demo.png,\n                     docs/programs-screenshots/brick-suspend-resume-demo.png,\n                     docs/programs-screenshots/brick-text-wrap-demo.png,\n                     docs/programs-screenshots/brick-theme-demo.png,\n                     docs/programs-screenshots/brick-viewport-scroll-demo.png,\n                     docs/programs-screenshots/brick-visibility-demo.png\n\nSource-Repository head\n  type:     git\n  location: git://github.com/jtdaugherty/brick.git\n\nFlag demos\n    Description:     Build demonstration programs\n    Default:         False\n\nlibrary\n  default-language:    Haskell2010\n  ghc-options:         -Wall -Wcompat -O2\n  default-extensions:  CPP\n  hs-source-dirs:      src\n  exposed-modules:\n    Brick\n    Brick.AttrMap\n    Brick.BChan\n    Brick.BorderMap\n    Brick.Focus\n    Brick.Forms\n    Brick.Main\n    Brick.Themes\n    Brick.Types\n    Brick.Util\n    Brick.Widgets.Border\n    Brick.Widgets.Border.Style\n    Brick.Widgets.Center\n    Brick.Widgets.Core\n    Brick.Widgets.Dialog\n    Brick.Widgets.Edit\n    Brick.Widgets.FileBrowser\n    Brick.Widgets.List\n    Brick.Widgets.ProgressBar\n    Brick.Widgets.Table\n    Data.IMap\n  other-modules:\n    Brick.Types.Common\n    Brick.Types.TH\n    Brick.Types.Internal\n    Brick.Widgets.Internal\n\n  build-depends:       base >= 4.9.0.0 && < 4.17.0.0,\n                       vty >= 5.36,\n                       transformers,\n                       data-clist >= 0.1,\n                       directory >= 1.2.5.0,\n                       dlist,\n                       exceptions >= 0.10.0,\n                       filepath,\n                       containers >= 0.5.7,\n                       microlens >= 0.3.0.0,\n                       microlens-th,\n                       microlens-mtl,\n                       config-ini,\n                       vector,\n                       contravariant,\n                       stm >= 2.4.3,\n                       text,\n                       text-zipper >= 0.12,\n                       template-haskell,\n                       deepseq >= 1.3 && < 1.5,\n                       unix,\n                       bytestring,\n                       word-wrap >= 0.2\n\nexecutable brick-table-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             TableDemo.hs\n  build-depends:       base,\n                       brick,\n                       text,\n                       vty\n\nexecutable brick-tail-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             TailDemo.hs\n  build-depends:       base,\n                       brick,\n                       text,\n                       vty,\n                       random\n\nexecutable brick-readme-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             ReadmeDemo.hs\n  build-depends:       base,\n                       brick,\n                       text\n\nexecutable brick-file-browser-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             FileBrowserDemo.hs\n  build-depends:       base,\n                       vty,\n                       brick,\n                       text\n\nexecutable brick-form-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             FormDemo.hs\n  build-depends:       base,\n                       brick,\n                       text,\n                       microlens,\n                       microlens-th,\n                       vty\n\nexecutable brick-text-wrap-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             TextWrapDemo.hs\n  build-depends:       base,\n                       brick,\n                       text,\n                       word-wrap\n\nexecutable brick-cache-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             CacheDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-visibility-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             VisibilityDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-viewport-scrollbars-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             ViewportScrollbarsDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-viewport-scroll-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  default-extensions:  CPP\n  main-is:             ViewportScrollDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-dialog-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             DialogDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-mouse-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             MouseDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th,\n                       text-zipper\n\nexecutable brick-layer-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             LayerDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-suspend-resume-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             SuspendAndResumeDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-cropping-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             CroppingDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-padding-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             PaddingDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-theme-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             ThemeDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-attr-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             AttrDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-list-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             ListDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       vector\n\nexecutable brick-list-vi-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             ListViDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       vector\n\nexecutable brick-custom-event-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             CustomEventDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-fill-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             FillDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-hello-world-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             HelloWorldDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-edit-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-language:    Haskell2010\n  main-is:             EditDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       vector,\n                       microlens >= 0.3.0.0,\n                       microlens-th\n\nexecutable brick-border-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-extensions:  CPP\n  default-language:    Haskell2010\n  main-is:             BorderDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-dynamic-border-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-extensions:  CPP\n  default-language:    Haskell2010\n  main-is:             DynamicBorderDemo.hs\n  build-depends:       base <= 5,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\nexecutable brick-progressbar-demo\n  if !flag(demos)\n    Buildable: False\n  hs-source-dirs:      programs\n  ghc-options:         -threaded -Wall -Wcompat -O2\n  default-extensions:  CPP\n  default-language:    Haskell2010\n  main-is:             ProgressBarDemo.hs\n  build-depends:       base,\n                       brick,\n                       vty,\n                       text,\n                       microlens\n\ntest-suite brick-tests\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      tests\n  ghc-options:         -Wall -Wcompat -Wno-orphans -O2\n  default-language:    Haskell2010\n  main-is:             Main.hs\n  other-modules:       List Render\n  build-depends:       base <=5,\n                       brick,\n                       containers,\n                       microlens,\n                       vector,\n                       vty,\n                       QuickCheck\n";
    }