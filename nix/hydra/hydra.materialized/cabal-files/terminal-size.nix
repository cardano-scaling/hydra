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
      identifier = { name = "terminal-size"; version = "0.3.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "matvey.aksenov@gmail.com";
      author = "Andreas Hammar, Matvey Aksenov";
      homepage = "";
      url = "";
      synopsis = "Get terminal window height and width";
      description = "Get terminal window height and width without ncurses dependency.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."process" or (errorHandler.buildDepError "process"));
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/terminal-size-0.3.3.tar.gz";
      sha256 = "8c174c8fa7200be2caffd6d25f789fd3c73f4b7b02989f332a42d7901fca60c3";
      });
    }) // {
    package-description-override = "name:                terminal-size\nversion:             0.3.3\nsynopsis:            Get terminal window height and width\ndescription:\n  Get terminal window height and width without ncurses dependency.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Andreas Hammar, Matvey Aksenov\nmaintainer:          matvey.aksenov@gmail.com\ncategory:            System\nbuild-type:          Simple\ncabal-version:       >= 1.10\nextra-source-files:\n  README.markdown\n  CHANGELOG.markdown\n\nsource-repository head\n  type:     git\n  location: https://github.com/biegunka/terminal-size\n\nsource-repository this\n  type:     git\n  location: https://github.com/biegunka/terminal-size\n  tag:      0.3.3\n\nlibrary\n  default-language:\n    Haskell2010\n\n  build-depends:\n    base >= 4 && < 5\n  if impl(ghc >= 7.2 && < 7.6)\n     build-depends:\n       ghc-prim\n  if os(windows)\n     build-depends:\n       process\n\n  build-tools:\n    hsc2hs\n  hs-source-dirs:\n    src\n  exposed-modules:\n    System.Console.Terminal.Size\n\n  other-modules:\n    System.Console.Terminal.Common\n  if os(Windows)\n    other-modules:\n      System.Console.Terminal.Windows\n  else\n    other-modules:\n      System.Console.Terminal.Posix\n\n  ghc-options:\n    -Wall\n    -fno-warn-unused-do-bind\n";
    }